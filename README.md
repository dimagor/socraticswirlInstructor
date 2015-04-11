Instructor dashboard for SocraticSwirl
======================================

Socratic Swirl lets instructors of the R programming language offer in-class, interactive programming exercises that the instructors view student answers and progress in real-time. This package lets instructors manage their exercises on the Socratic Swirl application, and launch a dashboard to watch their students' progress. See a demo of the dashboard [here](https://dgrtwo.shinyapps.io/socraticswirl/)!

### Installation and setup

Use the [devtools](https://github.com/hadley/devtools) package to install:

    devtools::install_github(c("rstudio/shinydashboard", "dgrtwo/rparse",
                               "dimagor/socraticswirlInstructor"))

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

### Creating Exercises

Exercises are created in SocraticSwirl the same way they are created for Swirl. [Here](http://swirlstats.com/instructors.html) you can read in detail about creating interactive exercises using the "swirlify" tool. In short, quiz questions are structured as [YAML](http://en.wikipedia.org/wiki/YAML) files, in a form something like this:

    - Class: cmd_question
      Output: Now store the contents of the 'cars$mpgCity' in a new variable called 'myMPG'.
      CorrectAnswer: myMPG <- cars$mpgCity
      AnswerTests: newcmd=myMPG <- cars$mpgCity
      Hint: Use the assignment operator to assign 'cars$mpgCity' to a new variable called
        'myMPG'.

Alternatively you could write a multiple choice question:

    - Class: mult_question
      Output: Mean, median, and mode are all measures of ____________.
      AnswerChoices: variation; significance; deviation; central tendency
      CorrectAnswer: central tendency
      AnswerTests: word=central tendency
      Hint: This is a fancy term for the "middle" of a dataset.

**Note**: in regular Swirl, questions are usually interspersed with text paragraphs and demonstrations. This is probably not the right approach for a set of SocraticSwirl exercises, since your students will be taking them in your class. Instead, make each of your items a question (either multiple choice or command-based).

### Uploading your exercises

Now you've created one or more lessons in your course, which you put into a directory called `my_course`. Suppose it has one lesson in it, called `my_lesson`. Time to upload it!

``` r
upload_course("my_course")
```

Your students can then install and take your lesson with the following line of code. *Make sure* they include the instructor username in the function, which allows you to view the results.

``` r
socratic_swirl("my_lesson", "my_course", instructor = "your_username")
```

Then to view the results in real-time, simply run the dashboard and select that course:

``` r
dashboard()
```
