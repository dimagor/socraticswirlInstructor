#' log in an instructor
#'
#' @export
socratic_swirl_instructor <- function(username, password, course = NULL) {
  options(socratic_swirl_instructor = username,
          socratic_swirl_course = course)
}


#' open dashboard
#'
#' @export
dashboard <- function() {
  app <- system.file("dashboard", package = "socraticswirlInstructor")
  shiny::runApp(app)
}
