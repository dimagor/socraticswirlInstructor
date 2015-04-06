#' Upload a Swirl course to the SocraticSwirl server
#'
#' Upload a Swirl course to the SocraticSwirl server. Courses can be
#' created as described here: http://swirlstats.com/instructors.html.
#'
#' @param directory path to directory of course to upload
#'
#' @details If you already have an existing course with this title, it will
#' be replaced.
#'
#' @export
upload_course <- function(directory) {
  u <- getOption("parse_user")
  if (is.null("parse_user")) {
    stop("Not signed in; use socratic_swirl_instructor()",
         "to sign in before uploading")
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

  # ensure others cannot alter exercises or courses
  private_acl <- ACL(public_write = FALSE)

  full_batched$ACL <- lapply(seq_len(nrow(full_batched)), function(i) private_acl)

  # delete any existing exercises
  existing_exercises <- parse_query("Exercise", course = course_title)
  if (!is.null(existing_exercises)) {
    message("Deleting ", nrow(existing_exercises), " existing exercises in ", course_title)
    parse_delete(existing_exercises)
  }
  parse_save(full_batched, "Exercise")

  # zip the file
  outzip <- ".forupload.zip"
  zip(outzip, directory)

  # upload the file, and delete temporary one
  f <- parse_file(paste0(course_title, ".zip"), outzip)

  existing_courses <- parse_query("Course", title = course_title)
  if (is.null(existing_courses)) {
    # create new zipfile
    co <- parse_object("Course", title = course_title, owner = u, zipfile = f,
                       ACL = private_acl)
  } else if (nrow(existing_courses) > 1) {
    stop("Multiple courses with this name; this should not happen")
  } else {
    # update existing course
    co <- as.parse_object(as.list(existing_courses[1, ]), "Course")
    co$zipfile <- f
    parse_save(co)
  }
  unlink(outzip)

  invisible()
}