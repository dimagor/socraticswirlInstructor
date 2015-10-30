#' sign up with an instructor account
#'
#' Sign up an account for teaching and uploading SocraticSwirl lessons.
#' Note that this requires an email confirmation.
#'
#' @param username desired username
#' @param password your password
#' @param email e-mail address, which will be sent a confirmation link
#'
#' @import rparse
#'
#' @export
socratic_swirl_signup <- function(username, password, email) {
  parse_signup(username, password, email = email)
  u <- parse_current_user()
  options(socratic_swirl_instructor = u$username)
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
socratic_swirl_instructor <- function(username = "default", password = "default", instance = "prod") {
    
  # For the instructor code for uploading courses, the keys are set here.
  # For the dashboard running on the server, they are set in server.R.
  # Both programs read the same key files.

  path = paste0("./keys-",instance,".R") # read keys from file in same directory as server.R script 

  source(path,local=TRUE) # username and password are set in key file

  parse_login(username, password)
  u <- parse_current_user()
  options(socratic_swirl_instructor = u$username)
}


#' Open the SocraticSwirl dashboard
#'
#' Opens the SocraticSwirl instructor dashboard in a browser.
#'
#' @param demo whether to launch a version with demo data
#'
#' @import shinydashboard
#' @import tidyr
#'
#' @export
dashboard <- function(demo = FALSE) {
  if (demo) {
    # set instructor to demo, while returning to original afterwards
    original_instructor <- getOption("socratic_swirl_instructor")
    options(socratic_swirl_instructor = "demo")
    on.exit(options(socratic_swirl_instructor = original_instructor))
  } else {
    u <- getOption("parse_user")
    if (is.null("parse_user")) {
      stop("Not signed in; use socratic_swirl_instructor()",
           "to sign in before starting the dashboard")
    }
  }

  app <- system.file("dashboard", package = "socraticswirlInstructor")
  shiny::runApp(app, host="128.112.102.51", port=8080, launch.browser = TRUE)
}
