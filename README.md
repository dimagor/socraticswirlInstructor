Instructor dashboard for SocraticSwirl
======================================

### Installation and setup

Use the [devtools](https://github.com/hadley/devtools) package to install:

    devtools::install_github(c("rstudio/shinydashboard", "dgrtwo/rparse",
                               "dimaoo7/socraticswirlInstructor"))

In order to use the dashboard or upload courses, you'll need an instructor
account with us. You can create one within R:

    library(socraticswirlInstructor)
    socratic_swirl_signup("your_name", "your_password", "your_email")

Once you've confirmed your email, you have an account! Each time you log into R
and want to use the dashboard or upload courses, you'll have to log in:

    socratic_swirl_login("your_name", "your_password")
