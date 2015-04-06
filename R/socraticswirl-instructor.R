#' sign up with an instructor account
#'
#' Sign up an account for teaching and uploading SocraticSwirl lessons.
#' Note that this requires an email confirmation.
#'
#' @param username desired username
#' @param password your password
#' @param email e-mail address, which will sent a confirmation link
#'
#' @import rparse
#'
#' @export
socratic_swirl_signup <- function(username, password, email) {
  parse_signup(username, password, email = email)
}


#' Log into an instructor account
#'
#' Log into an instructor account. This has to be done before accessing the
#' SocraticSwirl dashboard or submitting a new course.
#'
#' @param username account username
#' @param password account password
#'
#' @import rparse
#'
#' @export
socratic_swirl_instructor <- function(username, password) {
  u <- parse_login(username, password)
  options(socratic_swirl_instructor = u$username)
}


#' Upload a Swirl course to the SocraticSwirl server
#'
#' @param directory path to directory of course to upload
#'
#' @export
upload_course <- function(directory) {
  u <- getOption("parse_user")
  if (is.null("parse_user")) {
    stop("Not signed in; cannot upload")
  }

  course_title <- gsub("_", " ", basename(directory))

  ## add Exercise objects to database
  full_batched <- NULL
  lesson_dirs <- list.files(directory, full.names = TRUE, include.dirs = TRUE)

  if (length(lesson_dirs) == 0) {
    stop("No lessons found in course")
  }

  for (lesson_dir in lesson_dirs) {
    yaml_file <- file.path(lesson_dir, "lesson.yaml")
    if (!file.exists(yaml_file)) {
      stop("lesson.yaml not found in ", lesson_dir)
    }
    y <- yaml::yaml.load_file(yaml_file)

    lesson_name <- gsub("_", " ", basename(lesson_dir))

    batched <- dplyr::rbind_all(lapply(y[-1], as.data.frame, stringsAsFactors = FALSE))

    full_batched <- rbind(batched, full_batched)
  }

  full_batched <- dplyr::transmute(full_batched,
                                   course = course_title,
                                   lesson = lesson_name,
                                   exercise = seq_len(n()),
                                   prompt = Output,
                                   answer = as.character(CorrectAnswer),
                                   hint = Hint)

  # delete any existing exercises
  existing_exercises <- parse_query("Exercise", course = course_title)
  if (!is.null(existing_exercises)) {
    message("Deleting ", nrow(existing_exercises), " existing exercises in ", course_title)
    parse_delete(existing_exercises)
  }
  parse_save(full_batched, "Exercise")

  # zip the file
  outzip <- ".forupload.zip"
  zip(outzip, directory, "-q")

  # upload the file, and delete temporary one
  f <- parse_file(paste0(course_title, ".zip"), outzip)
  unlink(outzip)

  existing_courses <- parse_query("Course", title = course_title)
  if (is.null(existing_courses)) {
    # create new zipfile
    co <- parse_object("Course", title = course_title, owner = u, zipfile = f)
  } else if (nrow(existing_courses) > 1) {
    stop("Multiple courses with this name; this should not happen")
  } else {
    # update existing course
    co <- as.parse_object(as.list(existing_courses[1, ]), "Course")
    co$zipfile <- f
    parse_save(co)
  }

  ret <- NULL

  invisible(ret)
}


#' Open the SocraticSwirl dashboard
#'
#' Opens the SocraticSwirl instructor dashboard in a browser.
#'
#' @export
dashboard <- function() {
  app <- system.file("dashboard", package = "socraticswirlInstructor")
  shiny::runApp(app, launch.browser = TRUE)
}
