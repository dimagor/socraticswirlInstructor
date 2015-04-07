#' sign up with an instructor account
#'
#' Sign up an account for teaching and uploading SocraticSwirl lessons.
#' Note that this requires an email confirmation.
#'
#' @param username desired username
#' @param password your password
#' @param email e-mail address, which will be sent a confirmation link
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
socratic_swirl_instructor <- function(username, password) {
  parse_login(username, password)
  u <- parse_current_user()
  options(socratic_swirl_instructor = u$username)
}


#' Open the SocraticSwirl dashboard
#'
#' Opens the SocraticSwirl instructor dashboard in a browser.
#'
#' @import shinydashboard
#' @import tidyr
#'
#' @export
dashboard <- function() {
  u <- getOption("parse_user")
  if (is.null("parse_user")) {
    stop("Not signed in; use socratic_swirl_instructor()",
         "to sign in before starting the dashboard")
  }

  app <- system.file("dashboard", package = "socraticswirlInstructor")
  shiny::runApp(app, launch.browser = TRUE)
}
