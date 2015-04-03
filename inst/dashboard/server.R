library(shiny)
library(dplyr)
library(ggplot2)
library(swirl)

shinyServer(function(input, output, session) {
  instructor <- getOption("socratic_swirl_instructor")

  # Static Definitions ----------
  getPctColor <- function(pct){
    typeColors = c("black","red","orange","yellow","light-blue","navy","teal","aqua","lime","olive","green")
    typeColors[round(as.numeric(pct) / 10) + 1]
  }
  lectureInfo <- Parse_retrieve("lecdb_dima", instructor = instructor)

  # Reactive Functions ---------------
  usersLogged <- reactive({
    input$refresh #Refresh when button is clicked
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    Parse_retrieve("udb_dima") %>% .$student %>% unique %>% length
  })

  questionsAnswered <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    Parse_retrieve("adb_dima") %>% group_by()
  })

  lastUpdateTime <- reactive({
    usersLogged()
    Sys.time()
  })

  selectedExercise <- reactive({
    questionsAnswered() %>% filter(exercise == input$exerciseID)
  })

  exerciseTemporalTable <- reactive({
    exercise <- selectedExercise()
    data.frame(t_cut = seq(min(exercise$updatedAt), max(exercise$updatedAt) + 30, by = '30 sec')) %>%
      group_by(t_cut) %>%
      mutate(num_attempts = sum(distinct(group_by(exercise,student))$updatedAt <= t_cut),
             num_correct = sum(filter(distinct(group_by(exercise,student)),correct)$updatedAt <= t_cut)) %>%
      gather(type,num,-t_cut) %>% mutate(pct = num/usersLogged() * 100)
  })

  studentQuestions <- reactive({
    input$refresh
    interval <- max(as.numeric(input$interval), 5)
    if(input$interval != FALSE) invalidateLater(interval * 1000, session)
    Parse_retrieve("questdb_dima") %>% group_by()
  })


  # Header --------

  output$progressMenu <- renderMenu({
    progress_breakdown <- questionsAnswered() %>%
      group_by(exercise) %>%
      distinct(student, exercise, correct) %>%
      summarise(n=sum(correct)) %>%
      mutate(pct=round(n / usersLogged(), 2)*100)
    progress_breakdown <- left_join(lectureInfo,progress_breakdown, by="exercise") %>%
      mutate(pct=ifelse(is.na(pct),0,pct))
    progress_msgs <- apply(progress_breakdown, 1, function(row) {
      taskItem(value = row[["pct"]],
               color = getPctColor(row[["pct"]]),
               paste("Exercise:", row[["exercise"]])
      )
    })

    dropdownMenu(type = "tasks", .list = progress_msgs)
  })
  # Sidebar --------------
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
    exercises = as.list(lectureInfo$exercise)
    selectInput("exerciseID", label = NULL, exercises, selected = "1")
  })
  output$attemptedBar <- renderUI({
    attempted = selectedExercise() %>% distinct(student) %>% nrow
    #FIX: Error in eval(substitute(expr), envir, enclos) : incorrect length (0), expecting: 38,
    attempted_pct = round(attempted/usersLogged() * 100)
    taskItem(paste("Attempted:", attempted) , value = attempted_pct, color = getPctColor(attempted_pct))
  })

  output$completedBar <- renderUI({
    completed = selectedExercise() %>% filter(correct) %>% distinct(student) %>% nrow
    completed_pct = round(completed/usersLogged() * 100)
    taskItem(paste("Completed:", completed) , value = completed_pct, color = getPctColor(completed_pct))
  })

  output$exerciseQuestion <- renderText(
    lectureInfo %>% filter(exercise == input$exerciseID) %>% .$description
  )

  output$exerciseAnswer <- renderText(
    lectureInfo %>% filter(exercise == input$exerciseID) %>% .$desired_answer
  )

  output$incorrectAnswers <- renderDataTable(
    selectedExercise() %>% filter(!correct) %>% count(Answer=answer) %>% arrange(-n)
  )

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
             "timetracking" = ggplot(exercise_tt,aes(t_cut,pct, color = type)) +
               geom_path(cex=2) + ylim(0,100) +
               #10 minutes since first attempt or greater
               xlim(min(exercise_tt$t_cut),max(min(exercise_tt$t_cut) + 600,max(exercise_tt$t_cut))) +
               ylab("% of Students") + xlab("") +
               scale_color_discrete(name = "" ,labels = c("Attempted", "Completed")) +
               theme_classic() +
               theme(legend.justification=c(1,0), legend.position=c(1,0))
      )
    }
  })

  output$overviewGraph <- renderPlot({
    all_exercise_data <- questionsAnswered()
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
