Instructor dashboard for SocraticSwirl
======================================

Socratic Swirl lets instructors of the R programming language offer in-class, interactive programming exercises that the instructors view student answers and progress in real-time. This package lets instructors manage their exercises on the Socratic Swirl application, and launch a dashboard to watch their students' progress. See a demo of the dashboard [here](https://dgrtwo.shinyapps.io/socraticswirl/)!

### Installation and setup

Use the [devtools](https://github.com/hadley/devtools) package to install:

    devtools::install_github(c("rstudio/shinydashboard", "dgrtwo/rparse",
                               "dimaoo7/socraticswirlInstructor"))

In order to use the dashboard or upload courses, you'll need an instructor
account with us. You can create one within R:

    library(socraticswirlInstructor)
    socratic_swirl_signup("your_name", "your_password", "your_email")

Once you've confirmed your email, you have an account! Each time you restart R
and want to use the dashboard or upload courses, you'll have to log in:

    socratic_swirl_login("your_name", "your_password")

### Usage

Once you've logged in, you can access your Socratic Swirl dashboard with:

    dashboard()

This will show your students' progress and answering activity in real time. To view a demo, try:

    dashboard(demo = TRUE)

### Uploading

For information about uploading courses to the Socratic Swirl application, see the [vignette](vignettes/uploading_courses.Rmd).
