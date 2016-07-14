library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rparse)
library(socraticswirlInstructor)

# Load the keys for test and production by Sys.setenv
# source("./../../data/parse_keys.R")
data(parse_keys)
# Load the server instance, i.e. test vs production
# source("./../../data/server_instance.R")
data(server_instance)

if (server_instance == "test") {
  Sys.setenv(PARSE_APPLICATION_ID = PARSE_APPLICATION_ID_TEST)
  Sys.setenv(PARSE_API_KEY = PARSE_API_KEY_TEST)
} else {
  Sys.setenv(PARSE_APPLICATION_ID = PARSE_APPLICATION_ID_PROD)
  Sys.setenv(PARSE_API_KEY = PARSE_API_KEY_PROD)
}

parse_login(username, password)
u <- parse_current_user()
options(socratic_swirl_instructor = u$username)

#
# To get rid of the following warning message
#   shiny::renderDataTable is deprecated. Please use DT::renderDataTable instead.
#   To disable this message, run options(shiny.deprecation.messages=FALSE)
#   (Last used in version 0.11.1)
#
options(shiny.deprecation.messages=FALSE)

# Static Definitions ----------

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

# support functions for parse_queryAll

no_listcol <- function(df) {
  mask<-lapply(df[1,],function(x){!is.list(x)})
  df[,as.vector(mask,mode='logical')]
}
rbindx<-function(a,b) {rbind(no_listcol(a),no_listcol(b))}

parse_queryAll<-function(objName,...) {
# max skip allowed in parse API is 10000
  skip_amt<-1000
  skipcount<-skip_amt
  sum<-parse_query(objName,...)
  if (is.null(sum)) {NULL}
  else {
    if (nrow(sum) == skipcount) {
      repeat{
        part<-parse_query(objName,skip=skipcount,...)
        if (is.null(part))  # no more to read
	  {break}
        else {
          sum<-rbindx(sum,part)
          skipcount <- skipcount+skip_amt
	if (nrow(part) < skip_amt) # no more to read
	  {break}
        }
      }
    }
  }
  sum
}


getPctColor <- function(pct) {
  typeColors = c("black","red","orange","yellow","light-blue","navy","teal","aqua","lime","olive","green")
  typeColors[round(as.numeric(pct) / 10) + 1]
}

studentFactors <- function(students) {
  students %>% factor %>% unclass %>% paste("Student",.)
}

#
# The following are accessing the parse.com database.
# Only one instructor is assumed for now.
# If needed we may add instructor in the query.
#

# allStudents <- function() {
#   parse_queryAll("StudentList") %>% remove_df_columns() %>% distinct(email)
# }

# allExercises <- function() {
#   parse_queryAll("Exercise") %>% remove_df_columns()
# }

# allQuestions <- function() {
#   parse_queryAll("StudentQuestion") %>% remove_df_columns()
# }

#
# Read database per each student at a time. This solution takes longer time but will scale.
# Note: parse.com has query limit at 1000 and 10000 records.
#

# queryAllStudentResponses <- function() {
#   students <- allStudents()
#   if (!is.null(students)) {
#     sList <- unique(students$email)
#     sRes <- parse_queryAll('StudentResponse', student = sList[1])
#     for (i in 2:length(sList)) {
#       sRes = rbindx(sRes, parse_queryAll('StudentResponse', student = sList[i]) %>% remove_df_columns() )
#     }
#     sRes$isCorrect <- ifelse(sRes$command=='SKIPPED', NA, sRes$isCorrect)
#     sRes$lesson <- toupper(sRes$lesson)
#     sRes
#   } else {
#     NULL
#   }
# }



shinyServer(function(input, output, session) {
  current_course <- NULL
  current_lesson <- NULL

  instructor <- getOption("socratic_swirl_instructor")

  #
  # Initialize the global cache to hold database query results
  #
  # cacheStudents <<- allStudents()
  # cacheExercises <<- allExercises()
  # cacheQuestions <<- allQuestions()
  # cacheStudentResponses <<- parse_queryAllStudentResponses()

  #
  # input$refresh starts with 0, so initial updateFlag=0 accordingly
  #
  # updateFlag <<- 0
  # updateTime <<- Sys.time()

  # Reactive Functions ---------------

  updateTime <- reactive({
    input$refresh
    Sys.time()
  })

  cacheQuestions <- reactive({
    input$refresh
    parse_queryAll("StudentQuestion") %>% remove_df_columns()
  })

  cacheExercises <- reactive({
    input$refresh
    parse_queryAll("Exercise") %>% remove_df_columns()
  })

  cacheStudents <- reactive({
    input$refresh
    slist = parse_queryAll("StudentList") %>% remove_df_columns() %>% distinct(email)
    slist[slist$precept != "Dropped",]
  })

  cacheStudentResponses <- reactive({
    input$refresh
    students <- cacheStudents()
    if (!is.null(students)) {
      sList <- unique(students$email)
      sRes <- parse_queryAll('StudentResponse', student = sList[1])
      for (i in 2:length(sList)) {
        sRes = rbindx(sRes, parse_queryAll('StudentResponse', student = sList[i]) %>% remove_df_columns() )
      }
      sRes$isCorrect <- ifelse(sRes$command=='SKIPPED', NA, sRes$isCorrect)
      sRes$lesson <- toupper(sRes$lesson)
      sRes
    } else {
      NULL
    }
  })

  # Identify active courses
  activeCourses <- reactive({

    # Store IDs in global for persistence
    current_course <<- input$courseID
    current_lesson <<- input$lessonID
    current_precept <<- input$preceptID

    input$refresh #Refresh when button is clicked
    # updateCache()

    # active_courses = parse_queryAll("Exercise",instructor=instructor) %>% remove_df_columns()
    active_courses = cacheExercises()

    if (length(active_courses)>0) {
      active_courses %>% select(course, lesson) %>% distinct
    } else {
      data_frame(course = "NoStudents", lesson = "NoStudents")
    }
  })

  selectedLectureAll <- reactive({

    # selected_lecture <- parse_query("Exercise", course = input$courseID, lesson = input$lessonID) %>% remove_df_columns()

    if (is.null(input$courseID)) {
      if (is.null(input$lessonID)) {
        # select nothing since the courseID and lessonID are both not avaiable
        selected_lecture = NULL
      } else {
        selected_lecture = cacheExercises()
        selected_lecture = selected_lecture[selected_lecture$lesson == input$lessonID, ]
      }
    } else {
      selected_lecture = cacheExercises()
      selected_lecture = selected_lecture[selected_lecture$course == input$courseID, ]
      if (!is.null(input$lessonID)) {
        selected_lecture = selected_lecture[selected_lecture$lesson == input$lessonID, ]
      }
    }

    if (is.null(selected_lecture)) {
      NULL
    } else {
      # if(length(selected_lecture)>0) {
      if (nrow(selected_lecture)>0) {
        selected_lecture
      } else {
        NULL
      }
    }
  })

  selectedLectureAttempted <- reactive({
    # No need to remove df columns as they are done in the database queries
    #   selected_lecture <- studentResponses() %>% remove_df_columns()
    selected_lecture <- studentResponses()

    if (is.null(selected_lecture)) {
      NULL
    } else {
    #if(length(selected_lecture)>0) {
      if (nrow(selected_lecture)>0) {
        selected_lecture
      } else {
        NULL
      }
    }
  })

  selectedPrecept <- reactive({

    # selected_precept <- parse_queryAll("StudentList") %>% remove_df_columns()
	  selected_precept <- cacheStudents()

	  # if (length(selected_precept)>0) {
	  if (nrow(selected_precept)>0) {
	    selected_precept
	  } else {
	    NULL
	  }
  })

# make a student list
  studentList <- reactive({

    # student_list <- parse_queryAll("StudentResponse", course = input$courseID) %>% remove_df_columns() %>% distinct(student)

    studentRes = cacheStudentResponses()
    if (studentRes != NULL) {
      if (is.null(input$courseID)) {
        # nothing if courseID is presented
        student_list <- NULL
      } else {
        student_list <- studentRes[studentRes$course == input$courseID, ] %>% distinct(student)
      }
    } else {
      return(NULL)
    }

    # if (length(student_list)>0) {
	  if (nrow(student_list)>0) {
	     student_list }
	  else
	   { NULL }
  })

# list the StudentList object
  studentList2 <- reactive({

    #student_list2<-parse_queryAll("StudentList") %>% remove_df_columns() %>% distinct(email)
	  student_list2 <- cacheStudents()

	  if (input$preceptID == "All") {
	    # if (length(student_list2) > 0) {}
      if (nrow(student_list2) > 0)  {
	      student_list2
	    } else {
        NULL
	    }
	  } else {
	    if (is.null(input$preceptID)) {
        # choice to display all
        student_list2
	    } else {
	      student_list2[student_list2$precept == input$preceptID, ]
	    }
	  }
  })

  usersLogged <- reactive({
    students = cacheStudents()
    if (!is.null(input$preceptID)) {
        if (input$preceptID == "All") {
            length(students$email)
        } else {
            sum(students$precept == input$preceptID)
        }
    } else {
      # no preceptID defaults to all students
      length(students$email)
    }
  })

  studentResponses <- reactive({
    input$refresh
    # updateCache()

    #     student_responses <- parse_queryAll("StudentResponse",
    #                                   course = input$courseID,
    #                                   lesson = input$lessonID,
    #                                   instructor = instructor) %>% remove_df_columns()

    if (is.null(input$courseID)) {
      if (is.null(input$lessonID)) {
        # if courseID and lessonID are both not available, return nothing instead of everything
        #   student_responses = cacheStudentResponses
        student_responses = NULL
      } else {
        student_responses = cacheStudentResponses() %>% filter(lesson == input$lessonID)
      }
    } else {
      if (is.null(input$lessonID)) {
        student_responses = cacheStudentResponses() %>% filter(course == input$courseID)
      } else {
        student_responses = cacheStudentResponses() %>% filter(course == input$courseID, lesson == input$lessonID)
      }
    }

#    No need to do this as the cached studentResponses is already done with these
#    if (length(student_responses) > 0) {
#       student_responses$isCorrect <- ifelse(student_responses$command=='SKIPPED', NA, student_responses$isCorrect)
#       student_responses$lesson <- toupper(student_responses$lesson)
#    }

    # student_list <- parse_queryAll("StudentList") %>% remove_df_columns()
    student_list <- cacheStudents()

    if (is.null(student_responses)) return(NULL)
    if (is.null(student_list)) return(NULL)

    # if ((length(student_responses) > 0) & (length(student_list) > 0)) {
    if ((nrow(student_responses) > 0) & (nrow(student_list) > 0)) {
        merged_df <- merge(student_responses, student_list, by.x="student", by.y="email")
        names(merged_df)[names(merged_df) == 'updatedAt.x'] <- 'updatedAt'
        merged_df <- cbind(merged_df,paste(merged_df$first,merged_df$last))
	      colnames(merged_df)[length(names(merged_df))] <- "studentName"
        if (is.null(input$preceptID)) {
          NULL
        } else {
          if (input$preceptID == "All") {
            if (length(merged_df) > 0) {
              merged_df
            } else NULL
          } else {
          if (length(merged_df) > 0) {
            merged_df %>% filter(precept == input$preceptID)
          } else NULL
        }
      }
     } else NULL
  })

  selectedExercise <- reactive({
    student_responses <- studentResponses()
    # if (length(student_responses) > 0 ) {

    if (is.null(student_responses)) return(NULL)
    if (nrow(student_responses) > 0 ) {
      # student_responses %>% filter(exercise == input$exerciseID)
      if (is.null(input$exerciseID) || (input$exerciseID == "")) {
        NULL
      } else {
        student_responses %>% filter(exercise == input$exerciseID)
      }
    } else NULL
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
    # updateCache()

    # student_questions <- parse_queryAll("StudentQuestion",
    #                                course = input$courseID,
    #                                lesson = input$lessonID,
    #                                instructor = instructor) %>% remove_df_columns()

    if (is.null(input$courseID)) {
      if (is.null(input$lessonID)) {
        student_questions <- NULL
      } else {
        student_questions <- cacheQuestions()
        student_questions <- student_questions[student_questions$lesson == input$lessonID, ]
      }
    } else {
      student_questions <- cacheQuestions()
      student_questions <- student_questions[student_questions$course == input$courseID, ]
      if (!is.null(input$lessonID)) {
        student_questions <- student_questions[student_questions$lesson == input$lessonID, ]
      }
    }

    # if(length(student_questions)>0) student_questions else NULL
    if (is.null(student_questions)) return(NULL)
    if(nrow(student_questions)>0) student_questions else NULL
  })

  # Hubert's functions ----------------

allResponses <- reactive( {
	# studentResponses0 <- parse_queryAll("StudentResponse") %>% remove_df_columns()


  studentResponses0 <- cacheStudentResponses()
	# studentResponses0$lesson <- toupper(studentResponses0$lesson)
	students <- cacheStudents()
	studentResponses <- merge(studentResponses0, students, by.x="student", by.y="email")
        names(studentResponses)[names(studentResponses) == 'updatedAt.x'] <- 'updatedAt'
	studentResponses <- cbind(studentResponses,paste(studentResponses$first,studentResponses$last))
        colnames(studentResponses)[length(names(studentResponses))]<-"studentName"
	studentResponses$isCorrect <- ifelse(studentResponses$command=='SKIPPED',NA,studentResponses$isCorrect)

  if (is.null(input$preceptID)) {
    studentResponses
  } else {
    if (input$preceptID == "All") {
      studentResponses
    } else {
      studentResponses %>% filter(precept==input$preceptID)
    }
  }
})

uniqueSkipCount <- function(encodeSet) {
  decodeSet = strsplit(encodeSet, '---')
  cTime = Inf
  sTime = Inf
  for (i in decodeSet) {
    if (i[1] == "NA") {
      sTime = ifelse(i[2] < sTime, i[2], sTime)
    } else {
      if (i[1] == "TRUE") {
        cTime = ifelse(i[2] < cTime, i[2], cTime)
      }
    }
  }
  return(sTime < cTime)
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
    res = res[!is.na(res$lesson),]
    resTable = res[res$isCorrect == TRUE, ]
    tapply(resTable$isCorrect, list(as.factor(resTable$student), as.factor(resTable$lesson)), length)
    })

attemptTable <- reactive ({
    res <- allResponses()
    res = res[!is.na(res$lesson),]
    tapply(res$isCorrect, list(as.factor(res$student), as.factor(res$lesson)), length)
    })

timerTable <- reactive ({
    res <- allResponses()
    res = res[!is.na(res$lesson),]
    tapply(res$updatedAt, list(as.factor(res$student), as.factor(res$lesson)), minuteCount)
    })

uniqueRatioTable <- reactive ({
    res <- allResponses()
    res = res[!is.na(res$lesson),]
    round(100*uniqueSuccessTable()/uniqueAttemptTable(), digit=1)
    })

ratioTable <- reactive ({
    res <- allResponses()
    res = res[!is.na(res$lesson),]
    round(100*successTable()/attemptTable(), digit=1)
    })

uniqueCount <- function(a) {
    return(length(unique(a)))
    }

uniqueSuccessTable <-  reactive ({
    res <- allResponses()
    res = res[!is.na(res$lesson),]
    resTable = res[res$isCorrect == TRUE, ]
    tapply(resTable$ex, list(as.factor(resTable$student), as.factor(resTable$lesson)), uniqueCount)
    })

uniqueAttemptTable <- reactive ({
    res <- allResponses()
    res = res[!is.na(res$lesson),]
    tapply(res$ex, list(as.factor(res$student), as.factor(res$lesson)), uniqueCount)
    })

uniqueSkipTable <- reactive ({
  res <- allResponses()
  res = res[!is.na(res$lesson),]
  mtx = tapply(paste(res$isCorrect, res$updatedAt, sep="---"), list(as.factor(res$student), as.factor(res$lesson), as.factor(res$ex)), uniqueSkipCount)
  apply(mtx, c(1, 2), sum, na.rm=T)
})

unfinishedTable <- reactive ({
    atmp = uniqueAttemptTable()
    done = uniqueSuccessTable()
#    for (i in rownames(atmp)) {
#        for (j in colnames(atmp)) {
#	    if (!is.na(atmp[i, j])) {
#	      if (is.element(i, rownames(done)) && is.element(j, colnames(done))) {
#		      if (!is.na(done[i, j])) {
#		        atmp[i, j] = atmp[i, j] - done[i, j]
#		      }
#	       }
#	     }
#	   }
#	}
    done = done[rownames(atmp), colnames(atmp)]
    atmp = atmp - ifelse(is.na(done), 0, done)
    return(atmp)
    })


getNames <- function(s) {
    # students = parse_queryAll("StudentList")
    students = cacheStudents()

    names = 1:length(s)
    for (i in 1:length(s)) names[i] = paste(students$first[students$email == s[i]], students$last[students$email == s[i]])
    return(names)
    }

addNames <- function(aTable, d=1) {
    names = getNames(rownames(aTable))
    Average = apply(aTable, 2, mean, na.rm=T)
    temp = rbind(aTable, Average)
    Average = apply(temp, 1, mean, na.rm=T)
    aTable = round(cbind(temp, Average), digit=d)
    aTable = cbind(c(names, "Average"), aTable)
    colnames(aTable)[1]<-"Student"
    aTable
    }

  # Header --------

  output$progressMenu <- renderMenu({
    lectureInfo <- selectedLectureAll()
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
    if (is.null(input$courseID)) {
      # nothing is no course selected
      # lessons = active_courses %>% .$lesson %>% unique %>% as.list
      lessons = NULL
    } else {
      lessons = filter(active_courses, course == input$courseID) %>% .$lesson %>% unique %>% as.list
    }
    selectInput("lessonID", label = "Lesson:",
                lessons,
                selected = current_lesson)
  })

  output$usersessions <- renderUI({
    users_logged <- usersLogged()
    if(is.null(users_logged)) users_logged = 0
    h3("Students:", as.character(users_logged))
  })

  output$selectPrecept <- renderUI({
	preceptList <- selectedPrecept()
	plist <- sort(unique(append(preceptList$precept,c("All"),0 )))
  # plist = plist[plist != "Dropped"]
	selectInput("preceptID",label="Precept:",
  	  choices=plist,
	  selected = current_precept)
	})

  output$timeSinceLastUpdate <- renderText({
    paste("refreshed ", updateTime())
  })


  # BODY ------------
# select a student
  output$selectStudent <- renderUI({
	studentInfo <- studentList2()
#	if(!is.null(studentInfo)) students = as.list(sort(studentInfo$email))
	if(!is.null(studentInfo)) students = as.list(sort(paste(studentInfo$first,studentInfo$last)))
	else students = list()
	selectInput("studentID",label=NULL, students)
})
#end of select student

  output$selectExercise <- renderUI({
    lectureInfo <- selectedLectureAttempted()
    if(!is.null(lectureInfo)) exercises = as.list(sort(lectureInfo$exercise))
    else exercises = list()
    selectInput("exerciseID", label = NULL, exercises, selected = "1")
  })

  output$selectExercise2 <- renderUI({
    lectureInfo <- selectedLectureAttempted()
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
    lectureInfo <- selectedLectureAll()
    if(!is.null(lectureInfo)) {
      lectureInfo %>% filter(exercise == input$exerciseID) %>% .$prompt %>% h4
    } else {
      NULL
    }
  })

  output$exerciseAnswer <- renderText({
    lectureInfo <- selectedLectureAll()
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
     all_exercise_data$exercise<-ifelse(all_exercise_data$exercise>=10,all_exercise_data$exercise,paste0("0",all_exercise_data$exercise))
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
        #	filter(student == input$studentID) %>%
	      filter(paste(first,last) == input$studentID) %>%
	      arrange(updatedAt) %>%
	      select(Time = updatedAt, Lesson = lesson, Exercise = exercise, Command = command, Correct = isCorrect)
	   } else {
	      NULL
	   }
	})

  output$exerciseanswers <- renderDataTable({
    all_answers <- studentResponses()
    if (!is.null(all_answers)) {
      all_answers$updatedAt <- all_answers$updatedAt - 14400
      if (!is.null(input$exerciseID2)) {
	    all_answers %>%
      filter(exercise == input$exerciseID2) %>%
	    arrange(updatedAt) %>%
	    select(Time = updatedAt, Student= studentName, Lesson = lesson,  Command = command, Correct = isCorrect)
      } else {
        all_answers %>%
        arrange(updatedAt) %>%
        select(Time = updatedAt, Student= studentName, Lesson = lesson,  Command = command, Correct = isCorrect)
      }
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
  output$unfinishedTab <- renderDataTable(options = list(scrollX = TRUE),
  { addNames(unfinishedTable())
	})
  output$uniqueSkipTab <- renderDataTable(options = list(scrollX = TRUE),
  {  addNames(uniqueSkipTable())
  })
  output$uniqueSuccessTab <- renderDataTable(options = list(scrollX = TRUE),
  { addNames(uniqueSuccessTable())
	})
  output$uniqueAttemptTab <- renderDataTable(options = list(scrollX = TRUE),
  { addNames(uniqueAttemptTable())
	})
  output$ratioTab <- renderDataTable(options = list(scrollX = TRUE),
  { addNames(ratioTable())
  })
  output$uniqueRatioTab <- renderDataTable(options = list(scrollX = TRUE),
  { addNames(uniqueRatioTable())
  })
  output$successTab <- renderDataTable(options = list(scrollX = TRUE),
  { addNames(successTable())
  })
  output$attemptTab <- renderDataTable(options = list(scrollX = TRUE),
  { addNames(attemptTable())
  })
  output$timerTab <- renderDataTable(options = list(scrollX = TRUE),
  { addNames(timerTable())
	})


}
)
