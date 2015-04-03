#' sign up with an instructor account
#'
#' Sign up an account for teaching and uploading SocraticSwirl lessons.
#' Note that this requires an email confirmation.
#'
#' @param username desired username
#' @param password your password
#' @param email e-mail address, which will sent a confirmation link
#'
#' @export
socratic_swirl_signup <- function(username, password, email) {
  Parse_signup(username, password, email = email)
}


#' Log into an instructor account
#'
#' Log into an instructor account. This has to be done before accessing the
#' SocraticSwirl dashboard or submitting a new course.
#'
#' @param username account username
#' @param password account password
#'
#' @export
socratic_swirl_instructor <- function(username, password) {
  u <- Parse_login(username, password)
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

  # zip the file
  outzip <- ".forupload.zip"
  zip(outzip, directory)

  # upload the file
  f <- Parse_upload_file(paste0(basename(directory), ".zip"), outzip)

  # ret <- Parse_create("Course", title = course_title,
  #              owner = convert_pointer(u),
  #              zipfile = convert_pointer(f))
  # delete temporary zip file
  unlink(outzip)

  ## add Exercise objects
  for (lesson_dir in list.files(directory, full.names = TRUE, include.dirs = TRUE)) {
   yaml_file <- file.path(lesson_dir, "lesson.yaml")
   y <- yaml::yaml.load_file(yaml_file)

   lesson_name <- gsub("_", " ", basename(lesson_dir))

   for (i in 1:length(y)) {
     if (i > 1) {
       e <- y[[i]]
       Parse_create("Exercise",
                    course = course_title,
                    lesson = lesson_name,
                    exercise = i - 1,  # starts with metadata
                    prompt = e$Output,
                    answer = as.character(e$CorrectAnswer),
                    hint = e$Hint)
     }
   }
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
