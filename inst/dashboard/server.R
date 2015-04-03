library(shiny)
library(dplyr)
library(ggplot2)
library(swirl)

shinyServer(function(input, output, session) {
  current_course <- NULL
  current_lesson <- NULL

  instructor <- getOption("socratic_swirl_instructor")

  # Static Definitions ----------
  getPctColor <- function(pct){
    typeColors = c("black","red","orange","yellow","light-blue","navy","teal","aqua","lime","olive","green")
    typeColors[round(as.numeric(pct) / 10) + 1]
  }

  # Reactive Functions ---------------

  # Identify active courses
  activeCourses <- reactive({
    current_course <<- input$courseID
    current_lesson <<- input$lessonID

    input$refresh #Refresh when button is clicked
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    active_courses = Parse_retrieve("StudentSession", instructor = instructor)
    if(length(active_courses)>0)  active_courses %>% select(course,lesson) %>% distinct
    else data_frame(course = "NoStudents", lesson = "NoStudents")
  })

  selectedLecture <- reactive({
    selected_lecture <- Parse_retrieve("Exercise", course = input$courseID, lesson = input$lessonID)
    if(length(selected_lecture)>0) selected_lecture else NULL
  })

  usersLogged <- reactive({
    input$refresh #Refresh when button is clicked
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    users_logged <- Parse_retrieve("StudentSession", course = input$courseID, lesson = input$lessonID, instructor = instructor)
    if(length(users_logged) > 0) users_logged %>% .$student %>% unique %>% length
    else 0
  })

  studentResponses <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    Parse_retrieve("StudentResponse", course = input$courseID, lesson = input$lessonID, instructor = instructor)
  })

  lastUpdateTime <- reactive({
    usersLogged()
    Sys.time()
  })

  selectedExercise <- reactive({
    student_responses <- studentResponses()
    if ( length(student_responses) > 0 ) student_responses %>% filter(exercise == input$exerciseID)
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
        mutate(pct = value / users_logged * 100) %>%
        arrange(time, pct)
    }
    else NULL
  })

  studentQuestions <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    student_questions <- Parse_retrieve("StudentQuestions",
                                        course = input$courseID,
                                        lesson = input$lessonID,
                                        instructor = instructor)
    if(length(sstudent_questions)>0) student_questions else NULL
  })


  # Header --------

  output$progressMenu <- renderMenu({
    lectureInfo <- selectedLecture()
    student_responses <- studentResponses()
    if(length(student_responses)>0 & !is.null(lectureInfo)){
    progress_breakdown <- student_responses %>%
      group_by(exercise) %>%
      distinct(student, exercise, isCorrect) %>%
      summarise(n = sum(isCorrect)) %>%
      mutate(pct = n / usersLogged() * 100)
    progress_breakdown <- left_join(lectureInfo,progress_breakdown, by="exercise") %>%
      mutate(pct = ifelse(is.na(pct), 0, pct))
    }
    else progress_breakdown <- lectureInfo %>% mutate(pct=0)
    progress_msgs <- apply(progress_breakdown, 1, function(row) {
      taskItem(value = row[["pct"]],
               color = getPctColor(row[["pct"]]),
               paste("Exercise:", row[["exercise"]])
      )
    })

    dropdownMenu(type = "tasks", .list = progress_msgs)
  })

  # Sidebar --------------
  output$selectCourse <- renderUI({
    # h2("test")
    #Add warning if no student's registered yet
    active_courses = activeCourses()
    courses = as.list(unique(active_courses$course))
    selectInput("courseID", label = "Course:", choices = unique(active_courses$course),
                selected = current_course)
  })

  output$selectLesson <- renderUI({
    active_courses = activeCourses()
    lessons = filter(active_courses, course == input$courseID) %>% .$lesson %>% unique %>% as.list
    selectInput("lessonID", label = "Lesson:", lessons, selected = current_lesson)
  })

  output$usersessions <- renderUI({
      h3("Sessions:", as.character(usersLogged()))
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
  output$selectExercise <- renderUI({
    lectureInfo <- selectedLecture()
    exercises = as.list(lectureInfo$exercise)
    selectInput("exerciseID", label = NULL, exercises, selected = "1")
  })
  output$attemptedBar <- renderUI({
    selected_exercise = selectedExercise()
    if(!is.null(selected_exercise)) attempted = selected_exercise %>% distinct(student) %>% nrow
    else attempted = 0
    #FIX: Error in eval(substitute(expr), envir, enclos) : incorrect length (0), expecting: 38,
    attempted_pct = round(attempted/usersLogged() * 100)
    taskItem(paste("Attempted:", attempted) , value = attempted_pct, color = getPctColor(attempted_pct))
  })

  output$completedBar <- renderUI({
    selected_exercise = selectedExercise()
    if(!is.null(selected_exercise)) completed = selectedExercise() %>% filter(isCorrect) %>% distinct(student) %>% nrow
    else completed = 0
    completed_pct = round(completed/usersLogged() * 100)
    taskItem(paste("Completed:", completed) , value = completed_pct, color = getPctColor(completed_pct))
  })

  output$exerciseQuestion <- renderText({
    lectureInfo <- selectedLecture()
    if(!is.null(lectureInfo)) lectureInfo %>% filter(exercise == input$exerciseID) %>% .$prompt
    else NULL
  })

  output$exerciseAnswer <- renderText({
    lectureInfo <- selectedLecture()
    if(!is.null(lectureInfo)) lectureInfo %>% filter(exercise == input$exerciseID) %>% .$answer
    else NULL
  })

  output$incorrectAnswers <- renderDataTable({
    selected_exercise <- selectedExercise()
    if(!is.null(selected_exercise)) selected_exercise %>% filter(!isCorrect) %>% count(Command=command) %>% arrange(-n)
    else NULL
  })

  #TODO: Fun placeholder, make something sensible
  #NOTES: add switch dropdown for multiple plots, or consider gridextra
  output$exerciseGraph <- renderPlot({
    exercise_data <- selectedExercise()
    if(nrow(exercise_data) == 0 ) NULL
    else{
      exercise_tt <- exerciseTemporalTable()
      switch(input$exerciseGraphSelect,
             "attemptbar" = exercise_data %>% count(student) %>%
               count(attempts=factor(n)) %>%
               ggplot(aes(x = attempts, y = n, fill = attempts)) +
               geom_bar(stat = "identity", fill = "#6495ED") +
               coord_flip() +
               theme_minimal() +
               xlab("Attempts") + ylab("Frequency") +
               guides(fill = FALSE) +
               scale_fill_brewer(),
             "timetracking" = ggplot(exercise_tt,aes(time,pct, color = metric)) +
               geom_path(cex=2) + ylim(0,100) +
               #10 minutes since first attempt or greater
               xlim(min(exercise_tt$time),max(min(exercise_tt$time) + 600, max(exercise_tt$time))) +
               ylab("% of Students") + xlab("") +
               scale_color_discrete(name = "" ,labels = c("Attempted", "Completed")) +
               theme_classic() +
               theme(legend.justification=c(1,0), legend.position=c(1,0))
      )
    }
  })

  output$overviewGraph <- renderPlot({
    all_exercise_data <- studentResponses()
    if(nrow(all_exercise_data) == 0 ) NULL
    else{
      all_exercise_data %>% mutate(exercise=paste0("Exercise #",exercise)) %>%
      count(exercise,student) %>%
      select(-student) %>%
      mutate(n=as.character(n)) %>%
      count(exercise,attempts=n) %>%
      ggplot(aes(x = as.numeric(attempts), y = n, fill = as.numeric(attempts))) +
      geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ exercise) +
      theme_bw() +
      scale_x_discrete() +
      scale_y_discrete() +
      xlab("Attempts") + ylab("Frequency") +
      guides(fill = FALSE)
    }
  })

  output$questionsasked <- renderDataTable(
    studentQuestions() %>% arrange(desc(updatedAt)) %>% select(question, updatedAt, student)
  )

})
