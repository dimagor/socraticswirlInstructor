Instructor dashboard for SocraticSwirl
======================================

Socraticswirl, developed on top of swirl, lets instructors of the R programming language offer in-class, interactive programming exercises that the instructors view student answers and progress in real-time. This package lets instructors manage their exercises on the Socratic Swirl application, and launch a dashboard to watch their students' progress. See a demo of the dashboard [here](https://dgrtwo.shinyapps.io/socraticswirl/)!

Socraticswirl has three major components, the student software, a parse.com database, and the instructor software, i.e. the dashboard and course management utilities.

![alt text](release/img/SocraticSwirlFlowChart.png)

### Installation and setup

For more details, please download the [Socraticswirl Manual] (https://github.com/dimagor/socraticswirlInstructor/blob/master/release/Socraticswirl.docx).

Here are the steps:

    * Install SocraticswirlInstructor
    * Create parse.com applications
    * Set up Shiny servers, for test and production
    * Software Configuration
    * Load the student list
    * Load courses

##### Install SocraticswirlInstructor

Use the [devtools](https://github.com/hadley/devtools) package to install:

``` r
devtools::install_github(c("rstudio/shinydashboard", "dgrtwo/rparse", "dimagor/socraticswirlInstructor"))
```

The student side software is available at [socraticswirl](https://github.com/dimagor/socraticswirl). Students could install it on their personal computers.

##### Create parse.com applications

Register an account at parse.com for each group that will be independently using Socraticswirl.  Using that account, create two parse.com applications for each class, one for test and the other for production. Obtain Application Keys and REST API Keys for both apps.

There is a web interface to manage the database at parse.com, including the student list, the uploaded courses, and etc.

##### Set up Shiny servers

Set up and configurate the Shiny servers so that the dashboard applications can access the database at parse.com, and instructors can view the dashboard using browsers. The parse.com keys are needed here. It is important that two Shuny servers are set up, one for test and the other for production.

In order to use the dashboard or upload courses, you'll need an instructor
account with us. You can create one within R:

``` r
library(socraticswirlInstructor)
socratic_swirl_signup("your_name", "your_password", "your_email")
```

Once you've confirmed your email, you have an account! Each time you restart R
and want to use the dashboard or upload courses, you'll have to log in:

``` r
socratic_swirl_login("your_name", "your_password")
```

##### Software Configuration

The keys from parse.com are needed to configurate the software at both the student side (socraticswirl) and instructor side (socraticswirlInstructor). Each student will be assigned a unique id to initiate his/her socraticswirl software (https://github.com/dimagor/socraticswirl).

The Python program [uploadStudents.py] (https://github.com/dimagor/socraticswirlInstructor/blob/master/release/utility/uploadStudents.py) may be used to upload a student roster to the parse.com databases. Before the first use, you need to configurate it using the parse.com keys and application ids. 

##### Load the student list

There are two ways to create and manage the student list. 

    * The web interfact at parse.com
    * The utility Python function upLoadStudents.py

The upLoadStudents.py program is set up for two courses, referred to as “course1” and “course2”, and each has a test and a production database.  You need only use one of these.

To view the help text, use the “-h” flag:

    uploadStudents.py –h

To create the database schema (the first time) and upload a student roster, type the following:

    uploadStudents.py –create –filename student_roster.txt –add –n course1 –i test

This would upload the student list for course1 into the test database.  student_roster.txt should be a tab-separated file.  (You can save a tab-separated file in Excel by saving as a csv file.  Be sure not to save the file with quoted fields.) The expected format is one line for each student, as follows:

    lastname <tab> firstname <tab> email <tab> precept

##### Load courses
    
The first time you upload a course to either to the test or production server, you need to create an instructor names and passwords for the test and production respectively, as mentioned earlier in the Shiny server set up.

After that, you may upload the courses using the instructor names and passwords as follows:

``` r
library(socraticswirlInstructor)

# For the test instance:
socratic_swirl_instructor("<instructor name>", "instructor password", instance="test")

# or, for the production instance:
socratic_swirl_instructor("<instructor name>", "instructor password")

upload_course("/path/to/qss/swirl/INTRO")
upload_course("/path/to/qss/swirl/MEASUREMENT")
upload_course("/path/to/qss/swirl/CAUSALITY")
upload_course("/path/to/qss/swirl/DISCOVERY")
upload_course("/path/to/qss/swirl/PREDICTION")
```

### Usage

Once the Shiny server (s.univ.edu) runs, you can access your Socraticswirl dashboard with any browser:

    http://s.univ.edu/

We recommend that two servers are set up, one for test and the other for production. The production will be the one to monitor students' progress on the exercises for the class.

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

### Available swirl courses

There are swirl courses and/or lessons available at https://github.com/kosukeimai/qss-swirl. To get the course from the GitHub:

    git clone https://github.com/kosukeimai/qss

These courses are compatible for both swirl and socraticswirl. For people who are interested in making course compatible, CAUSALTY2 may be a good example where the data is stored accordingly by initLesson.R:

``` r
# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.


# Make path to lesson directory
lesson_dir <- file.path(path.package(substring(find("swirl")[1], 9)), "Courses",
                        "qss-swirl", "CAUSALITY2")

# Make path to data and let user call read.csv(data_path)
data_path <- file.path(lesson_dir, "resume.csv")
data_path2 <- file.path(lesson_dir, "social.csv")

# Load data into a variable for the user
resume <- read.csv(data_path, stringsAsFactors=FALSE)
social <- read.csv(data_path2, stringsAsFactors=FALSE)
```
