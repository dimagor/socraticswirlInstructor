library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rparse)
library(socraticswirlInstructor)

if (TRUE) {

  # before deploying to shinyapps, set to TRUE. Better way?

#  options(socratic_swirl_instructor = "demo")

#  Sys.setenv(PARSE_APPLICATION_ID = "C0pM75Sepnt5WhK6P6yhRA0TqVa6Xa3vqwZjpLfT",
#             PARSE_API_KEY = "HyXS1gEn6gf7gibjDJVWPYsnIoc0SXcp4mwohdmI")

options(socratic_swirl_instructor = "mcahn")
Sys.setenv(PARSE_APPLICATION_ID = "TEST-KEY", PARSE_API_KEY = "TEST-KEY")
#Sys.setenv(PARSE_APPLICATION_ID = "PROD-KEY", PARSE_API_KEY = "PROD-KEY")
Sys.setenv(TZ = "America/New York")
parse_login("INSTRUCTOR-NAME","INSTRUCTOR-PASSWORD")

}

# remove list columns from a table
remove_df_columns <- function(tbl) {
  if (is.null(tbl)) return(tbl)

  for (cn in names(tbl)) {
    if (is(tbl[[cn]], "data.frame")) {
      tbl[[cn]] <- NULL
    }
  }
  tbl
}


shinyServer(function(input, output, session) {
  current_course <- NULL
  current_lesson <- NULL

  instructor <- getOption("socratic_swirl_instructor")

  # Static Definitions ----------
  getPctColor <- function(pct){
    typeColors = c("black","red","orange","yellow","light-blue","navy","teal","aqua","lime","olive","green")
    typeColors[round(as.numeric(pct) / 10) + 1]
  }

  studentFactors <- function(students) students %>% factor %>% unclass %>% paste("Student",.)

  # Reactive Functions ---------------

  # Identify active courses
  activeCourses <- reactive({

    # Store IDs in global for persistence
    current_course <<- input$courseID
    current_lesson <<- input$lessonID
    current_precept <<- input$preceptID

    input$refresh #Refresh when button is clicked
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
#    active_courses = parse_query("StudentSession", instructor = instructor) %>% remove_df_columns()
    active_courses = parse_query("Exercise",instructor=instructor) %>% remove_df_columns()
    if (length(active_courses)>0) {
      active_courses %>% select(course, lesson) %>% distinct
    } else {
      data_frame(course = "NoStudents", lesson = "NoStudents")
    }
  })

  selectedLecture <- reactive({
    selected_lecture <- parse_query("Exercise", course = input$courseID, lesson = input$lessonID) %>% remove_df_columns()
    if(length(selected_lecture)>0) {
      selected_lecture
    } else {
      NULL
    }
  })

  selectedPrecept <- reactive({
	selected_precept <- parse_query("StudentList") %>%
	remove_df_columns()
	if (length(selected_precept)>0) {
	selected_precept
	} else {
	NULL
	}
})

# make a student list
  studentList <- reactive({
	student_list <- parse_query("StudentResponse", course = input$courseID) %>%
	remove_df_columns() %>%
	distinct(student) 
	if (length(student_list)>0) {
	   student_list }
	else 
	 { NULL }
})

# list the StudentList object
  studentList2 <- reactive({
	student_list2<-parse_query("StudentList") %>%
	remove_df_columns() %>%
	distinct(email) 
	if (input$preceptID == "All") {
   		if (length(student_list2) > 0)  {
		student_list2 }
		else
		  {NULL} }
	else {
            student_list2 %>% filter(precept==input$preceptID) }
})

  usersLogged <- reactive({
    input$refresh #Refresh when button is clicked	
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    users_logged <- parse_query("StudentSession", course = input$courseID, lesson = input$lessonID, instructor = instructor) %>% remove_df_columns()
    if(length(users_logged) > 0) users_logged %>% .$student %>% unique %>% length
    else NULL
  })

  studentResponses <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    student_responses <- parse_query("StudentResponse",
                                        course = input$courseID,
                                        lesson = input$lessonID,
                                        instructor = instructor) %>%
      remove_df_columns()
	student_list <- parse_query("StudentList") %>% 
	remove_df_columns()
	if (length(student_responses>0) & length(student_list)>0 ) {
	merged_df <- merge(student_responses, student_list, by.x="student", by.y="email")
        names(merged_df)[names(merged_df) == 'updatedAt.x'] <- 'updatedAt'
    if (input$preceptID == "All") {
	    if(length(merged_df)>0) merged_df else NULL } 
    else {
	if (length(merged_df) > 0) {
	    merged_df %>% filter(precept == input$preceptID) } 
        else NULL }
	}
	else NULL
  })

  lastUpdateTime <- reactive({
    usersLogged()
    Sys.time()
  })

  selectedExercise <- reactive({
    student_responses <- studentResponses()
    if ( length(student_responses) > 0 ) {
      student_responses %>% filter(exercise == input$exerciseID)
    }
    else NULL
  })

  exerciseTemporalTable <- reactive({
    exercise <- selectedExercise()
    users_logged <- usersLogged()
    if (!is.null(exercise)) {
      exercise %>%
        group_by(student) %>%
        summarize(correct = any(isCorrect),
                  first = min(updatedAt),
                  last = max(c(updatedAt[correct], 0))) %>%
        arrange(first) %>%
        mutate(attempted = seq_len(n())) %>%
        arrange(last) %>%
        mutate(answered = cumsum(correct)) %>%
        gather(type, time, first, last) %>%
        gather(metric, value, attempted, answered) %>%
        filter((metric == "attempted" & type == "first") |
                 metric == "answered" & type == "last",
               value > 0) %>%
        mutate(pct = (value / users_logged) * 100) %>%
        arrange(time, pct)
    }
    else NULL
  })

  studentQuestions <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    student_questions <- parse_query("StudentQuestion",
                                        course = input$courseID,
                                        lesson = input$lessonID,
                                        instructor = instructor) %>%
      remove_df_columns() 
    if(length(student_questions)>0) student_questions else NULL
  })

  # Hubert's functions ----------------

allResponses <- reactive( {
	studentResponses0 <- parse_query("StudentResponse") %>% remove_df_columns()
	students <- allStudents()
#	precept <- getPrecept(studentResponses0$student, students)

	studentResponses <- merge(studentResponses0, students, by.x="student", by.y="email")
        names(studentResponses)[names(studentResponses) == 'updatedAt.x'] <- 'updatedAt'

#	cbind(studentResponses, precept)  # Add a column of precept to the data frame of studentResponses
	if (input$preceptID == "All") {
		studentResponses }
	else {
	        studentResponses %>% filter(precept==input$preceptID) }


})

allStudents <- function() {
	students = parse_query("StudentList") %>% remove_df_columns() %>% distinct(email)
}

allExercises<- function() {
	exercises = parse_query("Exercise") %>% remove_df_columns() 
}

minuteCount <- function(aSet) {
    inSeconds = as.numeric(as.POSIXct(aSet))
    return(round((max(inSeconds) - min(inSeconds))/60, digits=1))
    }

getPrecept <- function(s, students) {
    precepts = 1:length(s)
    for (i in 1:length(s)) precepts[i] = students$precept[students$email == s[i]]
    return(precepts)
    }

successTable <- reactive ({
    res <- allResponses()
    tapply(res$isCorrect, list(as.factor(res$student), as.factor(res$lesson)), sum)
    })

attemptTable <- reactive ({
    res <- allResponses()
    tapply(res$isCorrect, list(as.factor(res$student), as.factor(res$lesson)), length)
    })

timerTable <- reactive ({
    res <- allResponses()
    tapply(res$updatedAt, list(as.factor(res$student), as.factor(res$lesson)), minuteCount)
    })

ratioTable <- reactive ({
    res <- allResponses()
    round(100*successTable()/attemptTable(), digit=1)
    })

getNames <- function(s) {
    students <- parse_query("StudentList")
    names = 1:length(s)
    for (i in 1:length(s)) names[i] = paste(students$first[students$email == s[i]], students$last[students$email == s[i]])
    return(names)
    }

addNames <- function(aTable, colName) {
    name <-  getNames(rownames(aTable)) 
    display_tab <- cbind(name, aTable)
    colnames(display_tab)[1]<-colName
    display_tab
    }


  # Header --------

  output$progressMenu <- renderMenu({
    lectureInfo <- selectedLecture()
    student_responses <- studentResponses()
    users_logged <- usersLogged()
    if(!is.null(student_responses) & !is.null(lectureInfo)){
      progress_breakdown <- student_responses %>%
        group_by(exercise) %>%
        distinct(student, exercise, isCorrect) %>%
        summarise(n = sum(isCorrect)) %>%
        mutate(pct = n / users_logged * 100) %>%
        mutate(pct = ifelse(pct > 100,100,pct))
      progress_breakdown <- left_join(lectureInfo,progress_breakdown, by="exercise") %>%
        mutate(pct = ifelse(is.na(pct), 0, pct)) %>% arrange(exercise)
      progress_msgs <- apply(progress_breakdown, 1, function(row) {
        taskItem(value = row[["pct"]],
                 color = getPctColor(row[["pct"]]),
                 paste("Exercise:", row[["exercise"]])
        )
      })
    }
    else progress_msgs = list()
    dropdownMenu(type = "tasks", .list = progress_msgs)
  })

  # Sidebar --------------
  output$selectCourse <- renderUI({
    #Add warning if no student's registered yet
    active_courses = activeCourses()
    courses = as.list(unique(active_courses$course))
    selectInput("courseID", label = "Course:",
                choices = unique(active_courses$course),
                selected = current_course)
  })

  output$selectLesson <- renderUI({
    active_courses = activeCourses()
    lessons = filter(active_courses, course == input$courseID) %>% .$lesson %>% unique %>% as.list
    selectInput("lessonID", label = "Lesson:",
                lessons,
                selected = current_lesson)
  })

  output$usersessions <- renderUI({
    users_logged <- usersLogged()
    if(is.null(users_logged)) users_logged = 0
    h3("Sessions:", as.character(users_logged))
  })

  output$selectPrecept <- renderUI({
    invalidateLater(5000, session)
	preceptList <- selectedPrecept()
	plist <- unique(append(preceptList$precept,c("All"),0 ))
	selectInput("preceptID",label="Precept:",
  	  choices=plist,
	  selected = current_precept)
	})

  output$timeSinceLastUpdate <- renderUI({
    # Trigger this every 5 seconds
    invalidateLater(5000, session)
    p(
      class = "text-muted",
      "Data refreshed ",
      round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
      " seconds ago."
    )
  })


  # BODY ------------
# select a student
  output$selectStudent <- renderUI({
	studentInfo <- studentList2()
	if(!is.null(studentInfo)) students = as.list(sort(studentInfo$email))
	else students = list()
	selectInput("studentID",label=NULL, students)
})
#end of select student

  output$selectExercise <- renderUI({
    lectureInfo <- selectedLecture()
    if(!is.null(lectureInfo)) exercises = as.list(sort(lectureInfo$exercise))
    else exercises = list()
    selectInput("exerciseID", label = NULL, exercises, selected = "1")
  })

  output$selectExercise2 <- renderUI({
    lectureInfo <- selectedLecture()
    if(!is.null(lectureInfo)) exercises = as.list(sort(lectureInfo$exercise))
    else exercises = list()
    selectInput("exerciseID2", label = NULL, exercises, selected = "1")
  })

  output$attemptedBar <- renderUI({
    selected_exercise = selectedExercise()
    if(!is.null(selected_exercise)){
      attempted = selected_exercise %>% distinct(student) %>% nrow
      attempted_pct = round(attempted/usersLogged() * 100)
      if (attempted_pct > 100) attempted_pct = 100 #Temp fix
    }
    else{
      attempted = 0
      attempted_pct = 0
    }
    # color = getPctColor(attempted_pct)
    taskItem(paste("Attempted:", attempted) , value = attempted_pct, color = "red")
  })

  output$completedBar <- renderUI({
    selected_exercise = selectedExercise()
    if(!is.null(selected_exercise)){
      completed = selectedExercise() %>% filter(isCorrect) %>% distinct(student) %>% nrow
      completed_pct = round(completed/usersLogged() * 100)
      if (completed_pct > 100) completed_pct = 100 #Temp fix
    }
    else {
      completed = 0
      completed_pct = 0
    }
    # color = getPctColor(completed_pct)
    taskItem(paste("Completed:", completed) , value = completed_pct, color = "blue")
  })

  output$exerciseQuestion <- renderUI({
    lectureInfo <- selectedLecture()
    if(!is.null(lectureInfo)) {
      lectureInfo %>% filter(exercise == input$exerciseID) %>% .$prompt %>% h4
    } else {
      NULL
    }
  })

  output$exerciseAnswer <- renderText({
    lectureInfo <- selectedLecture()
    if(!is.null(lectureInfo)) {
      lectureInfo %>% filter(exercise == input$exerciseID) %>% .$answer
    } else {
      NULL
    }
  })


  output$incorrectAnswers <- renderDataTable(
    options = list(
      lengthChange=FALSE, pageLength = 20,
      searching = FALSE,
      ordering = FALSE),{
    selected_exercise <- selectedExercise()
    if(!is.null(selected_exercise)){
      selected_exercise <- selected_exercise %>%
      filter(!isCorrect) %>%
      select("Submitted Command" = command, "Error Message" = errorMsg, TimeSubmitted = updatedAt) %>% arrange(desc(TimeSubmitted))   # adjusted for join-CA
      # selected_exercise[order(selected_exercise[[input$incorrectSort]],decreasing = TRUE), ]
      }

    else NULL
  })

  output$commonErrors <- renderDataTable(
    options = list(
      lengthChange=FALSE, pageLength = 20,
      searching = FALSE,
      ordering = FALSE),{
        selected_exercise <- selectedExercise()
        if(!is.null(selected_exercise)){
          selected_exercise <- selected_exercise %>%
            filter(!isCorrect, isError) %>%
            select(ErrorMessage = errorMsg) %>%
            count(ErrorMessage) %>% arrange(desc(n))
        }

        else NULL
      })

  output$plotFreqAttempts <- renderPlot({
    exercise_data <- selectedExercise()
    if(!is.null(exercise_data)){
      exercise_data %>% count(student) %>%
      count(attempts=factor(n)) %>%
      ggplot(aes(x = attempts, y = n, fill = attempts)) +
      geom_bar(stat = "identity", fill = "#6495ED") +
      theme_minimal() +
      xlab("Number of Attempts") + ylab("Frequency") +
      guides(fill = FALSE) +
      scale_fill_brewer()
    }
    else NULL
  })

  output$plotProgress <- renderPlot({
    exercise_tt <- exerciseTemporalTable()
    if(!is.null(exercise_tt)){
      ggplot(exercise_tt,aes(time,pct, color = metric)) +
      geom_path(cex=2) + ylim(0,100) +
      #2 minutes since first attempt or greater
      xlim(min(exercise_tt$time),max(min(exercise_tt$time) + 120, max(exercise_tt$time))) +
      ylab("% of Students") + xlab("") +
      scale_color_discrete(name = "" ,labels = c("Attempted", "Completed")) +
      theme_classic() +
      theme(legend.justification=c(1,0), legend.position=c(1,0))
    }
    else NULL
  })

  output$overviewGraph <- renderPlot({
    all_exercise_data <- studentResponses()
    if(is.null(all_exercise_data) ) NULL
    else{
      all_exercise_data %>% mutate(exercise=paste0("Exercise #",exercise)) %>%
        count(exercise,student) %>%
        select(-student) %>%
        mutate(n=as.character(n)) %>%
        count(exercise,attempts=n) %>%
        ggplot(aes(x = as.numeric(attempts), y = n, fill = as.numeric(attempts))) +
        geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ exercise) +
        theme_light() +
        scale_y_discrete() +
        xlab("Attempts") + ylab("Frequency") +
        guides(fill = FALSE)
    }
  })

  output$questionsasked <- renderDataTable({
    student_questions <- studentQuestions()
    if(!is.null(student_questions)) {
      student_questions %>%
      mutate(student = studentFactors(student)) %>%
      arrange(desc(updatedAt)) %>%
      select(Questions = question, Student = student, Time = updatedAt)
    } else {
      NULL
    }
  })

  output$studentanswers <- renderDataTable({
    all_answers <- studentResponses()
    if (!is.null(all_answers)) {
        all_answers$updatedAt <- all_answers$updatedAt - 14400
	all_answers %>%
	filter(student == input$studentID) %>%
	arrange(updatedAt) %>%
	select(Time = updatedAt, Lesson = lesson, Exercise = exercise, Command = command, Correct = isCorrect)  # changed for Precept - CA
	} else {
	NULL
	}	
	})

  output$exerciseanswers <- renderDataTable({
    all_answers <- studentResponses()
    if (!is.null(all_answers)) {
        all_answers$updatedAt <- all_answers$updatedAt - 14400
	all_answers %>%
	filter(exercise == input$exerciseID2) %>%
	arrange(updatedAt) %>%
	select(Time = updatedAt, Student= student, Lesson = lesson,  Command = command, Correct = isCorrect)
	} else {
	NULL
	}	
	})

  output$studentlist2 <- renderDataTable({
	all_students <- studentList2()
	if (!is.null(all_students)) {
	all_students %>%
	arrange(last,first) %>%
	select(FirstName = first,LastName = last, Email = email, Precept = precept)
	} else {
	NULL
	}
	})
  output$ratioTab <- renderDataTable({	
	addNames(ratioTable(),c("Student"))
	})
  output$successTab <- renderDataTable({
	addNames(successTable(),c("Student"))
	})
  output$attemptTab <- renderDataTable({
	addNames(attemptTable(),c("Student"))
	})
  output$timerTab <- renderDataTable({
	addNames(timerTable(),c("Student"))
	})


}
)

