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
    menuItem("Submitted Questions", tabName = "questions_tab", icon = icon("question-circle"))
  ),
  p(), #Fix for better separation
  hr(),
  box(style = "color: black;",
      width = NULL, title = "Controls", collapsible = TRUE,
      uiOutput("selectCourse"),
      uiOutput("selectLesson"),
      selectInput("interval", label = "Refresh interval",
                  choices = c(
                    "5 seconds" = 5,
                    "15 seconds" = 15,
                    "30 seconds" = 30,
                    "1 minute" = 50,
                    "5 minutes" = 600,
                    "Off" = FALSE),
                  selected = "30"),
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
                     # Question Info
                     box(collapsible = FALSE, width = NULL, title = "Question Info",
                         verbatimTextOutput("exerciseQuestion"),
                         verbatimTextOutput("exerciseAnswer")),

                     # TODO: Capture Filter/Sort and use as input for tableoutput to preserve post refresh
                     # Answer Table
                     box(collapsible = FALSE, width = NULL, title = "Incorrect Answers",
                         selectInput("incorrectSort", label = "Sort Column:", width = "50%",
                                     choices = c("updatedAt", "command", "isError", "errorMsg"),
                                     selected = "updatedAt"),
                         # checkboxInput("incorrectSortDescending", label = "Descending", value = TRUE),
                         dataTableOutput("incorrectAnswers"))
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
    )
  )
)

dashboardPage(header,  sidebar,  body, skin = "blue")
