context("Uploading courses")

library(rparse)

classes <- c("_User", "Course", "Exercise")

if (parse_setup_test(to_remove = classes, error = FALSE)) {
  test_course <- system.file("test_course", package = "socraticswirlInstructor")

  test_that("Can sign up and log in", {
    u <- socratic_swirl_signup("instructor1", "12345", "example@example.com")
    expect_equal(getOption("socratic_swirl_instructor"), "instructor1")

    u2 <- socratic_swirl_signup("instructor2", "67890", "example2@example.com")
    expect_equal(getOption("socratic_swirl_instructor"), "instructor2")

    socratic_swirl_instructor("instructor1", "12345")
    expect_equal(getOption("socratic_swirl_instructor"), "instructor1")
  })

  test_that("Can upload a course", {
    parse_logout()
    # need to be logged in to upload
    expect_error(upload_course(test_course), "Not signed in")

    socratic_swirl_instructor("instructor1", "12345")
    upload_course(test_course)
    # check that course can be updated
    expect_message(upload_course(test_course), "Deleting 2 existing exercises")

    # ensure that someone else cannot modify that course
    socratic_swirl_instructor("instructor2", "67890")
    expect_error(upload_course(test_course), "title is taken")

    # check that the exercises are uploaded
    socratic_swirl_instructor("instructor1", "12345")
    existing_exercises <- parse_query("Exercise", course = "test course")

    expect_equal(nrow(existing_exercises), 2)
    expect_true(all(existing_exercises$course == "test course"))
    expect_true(all(existing_exercises$lesson == "test lesson"))
    expect_true(all(existing_exercises$instructor == "instructor1"))

    # confirm that the course can be installed with socraticswirl
    # (these tests overlap those for the socraticswirl package)
    socraticswirl::install_course_socratic_swirl("test course")

    # return back to original state
    parse_restore_state(to_remove = classes)
  })
}
