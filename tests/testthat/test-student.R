context("Student exercises")

library(testthat)
library(rparse)

# these tests partly test the socraticswirl package. This will have to do
# since they depend on each other.

# set up Parse environment
classes <- c("_User", "Course", "Exercise", "StudentResponse", "StudentSession")

if (require("socraticswirlInstructor", quietly = TRUE) &&
    parse_setup_test(to_remove = classes, error = FALSE)) {
  # these tests depend on functionality tested in test-upload
  # set up the test course
  test_course <- system.file("test_course", package = "socraticswirlInstructor")

  # log in as an instructor
  socratic_swirl_signup("instructor1", "12345", "example@example.com")
  upload_course(test_course)

  # set up a student
  # this is the next issue to fix; something with installation
  # socraticswirl::socratic_swirl("test lesson", "test course", "instructor1")

  parse_restore_state(to_remove = classes)
}
