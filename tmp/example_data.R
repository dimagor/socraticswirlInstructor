library(digest)
source("~/.Rprofile")

# # Student Session ---------------
# #
# # Fields: course, lesson, instructor, student
#
for(i in seq(25)) Parse_create("StudentSession",
                                       course="default",
                                       lesson="ggplot",
                                       instructor="dima",
                                       student=digest(i))
for(i in seq(25)) Parse_create("StudentSession",
                                       course="default",
                                       lesson="vectors",
                                       instructor="dima",
                                       student=digest(i))
for(i in seq(25)) Parse_create("StudentSession",
                                       course="default",
                                       lesson="ggplot",
                                       instructor="dgrtwo",
                                       student=digest(i))





# Student Response ----------------------
# # Fields: course, lesson, instructor, student, command, exercise, isCorrect, isError, errorMsg


simulateStudents <- function(answerpool,  #Assume first is correct
                             exercise = 1,
                             course = "default",
                             lesson = "ggplot",
                             instructor = "dima",
                             totalstudents = 25,
                             sampledstudents = 15,
                             attemptbuffer = 2,
                             maxsleep = 20,
                             commonErrors = c("Variable not found", "Package not installed", "Data not loaded")){
  for(s in sample(seq(totalstudents), sampledstudents)) {
    for(a in seq(sample(length(answerpool) + attemptbuffer, 1))){ #attempts
      command = sample(answerpool, 1)
      isCorrect = command == answerpool[1]
      isError = ifelse(command != answerpool[1],sample(c(TRUE,FALSE),1),FALSE)
      errorMsg = if (isError) sample(commonErrors,1) else NULL
      Parse_create("StudentResponse",
                           course = course,
                           lesson = lesson,
                           instructor = instructor,
                           student = digest(s),
                           exercise = exercise,
                           isCorrect = isCorrect,
                           command = command,
                           isError =  isError,
                           errorMsg = errorMsg)
      if(isCorrect) break
      pause = sample(seq(1,maxsleep),1)
      print(paste("Waiting ", as.character(pause)))
      Sys.sleep(pause) #For time series graphs
    }
  }
}


q1_ans <- c("library(ggplot2)", "library(ggplot2)", #To improve chances
            "libary(ggplot2)","library(ggplot)","library ggplot", "library gplot", "lib(ggplot2)")
simulateStudents(answerpool = q1_ans, exercise = 1, maxsleep = 10)
simulateStudents(answerpool = q1_ans, exercise = 1, maxsleep = 10,instructor = "dgrtwo", sampledstudents = 8)

q2_ans <- c("count(diamonds,color)", "count(diamonds,color)",
            "counts(diamonds, color)",
            "count(diamonds)",
            "count(diamonds, clarity)",
            "for(i in factor(diamonds$color)) print(color)",
            "ggplot(diamonds,aes(color,price)) + geom_boxplot()",
            "count diamond,color",
            "countit(diamond,color)",
            "Up, Up, Down, Down, Left, Right, Left, Right, B, A, Start"
            )

simulateStudents(answerpool = q2_ans, exercise = 2, maxsleep = 12, sampledstudents = 7)




# Lecture DB --------------
Parse_create("Exercise",course="default",lesson="ggplot", exercise=1, prompt="Load ggplot library", answer="library(ggplot2)")
Parse_create("Exercise",course="default",lesson="ggplot", exercise=2, prompt="Count the diamonds db by color", answer="count(diamonds,color)")
Parse_create("Exercise",course="default",lesson="ggplot", exercise=3, prompt="Plot diamonds: carat vs price", answer="ggplot(diamonds,aes(carat,price))+geom_point()")
Parse_create("Exercise",course="default",lesson="ggplot", exercise=4, prompt="Plot diamonds: carat vs price with color breakdown", answer="ggplot(diamonds,aes(carat,price,color=color))+geom_point()")

Parse_create("Exercise",course="default",lesson="vectors", exercise=1, prompt="Use seq to make a vector 1-10", answer="seq(10)")

# QuestionDB
questions <- c("Whats your (full) name?", "How old are you?", "Whats your Birthday?", "What starsign does that make it?", "Whats your favourite colour?", "Whats your lucky number?", "Do you have any pets?", "Where are you from?", "How tall are you?", "What shoe size are you?", "How many pairs of shoes do you own?")

for(i in questions) Parse_create("StudentQuestion",course="default",lesson="ggplot", instructor="dima", student = digest(sample(1:10,1)), addressed = FALSE, question = i)
