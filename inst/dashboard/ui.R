library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "SocraticSwirl",
                          dropdownMenuOutput("progressMenu"))

sidebar <- dashboardSidebar(
  uiOutput("usersessions"),
  hr(),
  sidebarMenu(
    menuItem("Exercise Dashboard", tabName = "exercise_tab", icon = icon("dashboard")),
    menuItem("Lesson Overview", tabName = "overview_tab", icon = icon("list")),
    menuItem("Submitted Questions", tabName = "questions_tab", icon = icon("question-circle")),
    menuItem("Student Dashboard",tabName = "success_tab", icon=icon("list")),
    menuItem("Response Details",tabName = "details_tab", icon = icon("list"))
  ),
  p(), #Fix for better separation
  hr(),
  box(style = "color: black;",
      width = NULL, title = "Controls", collapsible = TRUE,
      uiOutput("selectCourse"),
      uiOutput("selectLesson"),
      uiOutput("selectPrecept"),
      selectInput("interval", label = "Refresh interval",
                  choices = c(
#                    "5 seconds" = 5,
#                    "15 seconds" = 15,
#                    "30 seconds" = 30,
#                    "1 minute" = 50,
                    "5 minutes" = 600,
		    "15 minutes" = 1800,
	            "30 minutes" = 3600
#                    ,"Off" = FALSE
		    ),
                  selected = "3600"),

      uiOutput("timeSinceLastUpdate"),
      actionButton("refresh", "Refresh now")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "exercise_tab",
            fluidRow(
              # Left Column
              column(width = 6,
                     # Exercise Selector & Progress
                     box(collapsible = FALSE, width = NULL, title = "Select Exercise",
                         uiOutput("selectExercise"),
                         uiOutput("attemptedBar", style = "list-style-type: none;"),
                         uiOutput("completedBar", style = "list-style-type: none;")),
                     # Plots
                     tabBox(width = NULL,
                       tabPanel(title = "Attempt Frequency",
                                plotOutput("plotFreqAttempts")),
                       tabPanel(title = "Progress Tracking",
                                plotOutput("plotProgress"))
                     )
              ),

              # Right Column
              column(width = 6,
                     # Exercise Info
                     tabBox(width = NULL,
                     tabPanel(title = "Exercise Prompt",
                         uiOutput("exerciseQuestion")),
                     tabPanel(title = "Correct Answer",
                        verbatimTextOutput("exerciseAnswer"), collapsible = TRUE)
                     ),

                     # Answer Table
                     tabBox(width = NULL,
                            tabPanel(title = "Incorrect Answers",
                                     # selectInput("incorrectSort", label = "Sort Column:", width = "50%",
                                     # choices = c("updatedAt", "command", "isError", "errorMsg"),
                                     # selected = "updatedAt"),
                                     # checkboxInput("incorrectSortDescending", label = "Descending", value = TRUE),
                                     dataTableOutput("incorrectAnswers")),
                            tabPanel(title = "Common Errors",
                                     dataTableOutput("commonErrors")
                            )
                     )
              )
            )
    ),

    tabItem(tabName = "overview_tab",
            box(collapsible = TRUE, width = NULL,
                plotOutput("overviewGraph"))
    ),
    tabItem(tabName = "questions_tab",
            box(width = NULL,
                dataTableOutput("questionsasked")
                )
	),

    tabItem(tabName = "details_tab",
	tabBox(width = NULL
	,tabPanel(title = "Select Exercise"
		,uiOutput("selectExercise2")
		,dataTableOutput("exerciseanswers") )
	,tabPanel(title = "Select Student"
	   	,uiOutput("selectStudent")
   	    	,dataTableOutput("studentanswers") )
	)),

    tabItem(tabName = "success_tab",
	tabBox(width = NULL
	,tabPanel(title = "Unique Attempt"
	        ,dataTableOutput("uniqueAttemptTab") )
	,tabPanel(title = "Unique Success"
		      ,dataTableOutput("uniqueSuccessTab") )
	,tabPanel(title = "Success Ratio"
	          ,dataTableOutput("uniqueRatioTab") )
	,tabPanel(title = "Incomplete Attempt"
	          ,dataTableOutput("unfinishedTab") )
	,tabPanel(title = "Total Time"
	        ,dataTableOutput("timerTab") )
	,tabPanel(title = "Attempt Counts"
	          ,dataTableOutput("attemptTab") )
	,tabPanel(title = "Success Counts"
	          ,dataTableOutput("successTab") )
	,tabPanel(title = "Overall Success Ratio"
	          ,dataTableOutput("ratioTab") )
	))
    )
)

dashboardPage(header,  sidebar,  body, skin = "blue")
