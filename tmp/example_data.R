library(digest)
source("~/.Rprofile")

#
# # AnswerDB
# #
# # adb_dima
# #
# # Fields: course, lesson, instructor, student, exercise, correct, answer

# # Lecture DB --------------
# swirl:::Parse_create("lecdb_dima",course="default",lesson="ggplot", instructor="dima", exercise=1, description="Load ggplot library", desired_answer="library(ggplot2)")
# swirl:::Parse_create("lecdb_dima",course="default",lesson="ggplot", instructor="dima", exercise=2, description="Count the diamonds db by color", desired_answer="count(diamonds,color)")
# swirl:::Parse_create("lecdb_dima",course="default",lesson="ggplot", instructor="dima", exercise=3, description="Plot diamonds: carat vs price", desired_answer="ggplot(diamonds,aes(carat,price))+geom_point()")
# swirl:::Parse_create("lecdb_dima",course="default",lesson="ggplot", instructor="dima", exercise=4, description="Plot diamonds: carat vs price with color breakdown", desired_answer="ggplot(diamonds,aes(carat,price,color=color))+geom_point()")

# QuestionDB
questions <- c("Whats your (full) name?", "How old are you?", "Whats your Birthday?", "What starsign does that make it?", "Whats your favourite colour?", "Whats your lucky number?", "Do you have any pets?", "Where are you from?", "How tall are you?", "What shoe size are you?", "How many pairs of shoes do you own?")

for(i in questions) swirl:::Parse_create("questdb_dima",course="default",lesson="ggplot", instructor="dima", student = digest(sample(1:10,1)), addressed = FALSE, question = i)


#
# # Participant DB
# #
# # swirl:::Parse_create()
# #
# # udb_dima
# #
# # Fields: course, lesson, instructor
#

# for(i in seq(25)) swirl:::Parse_create("udb_dima",course="default",lesson="ggplot", instructor="dima", student=digest(i))


simulateStudents <- function(answerpool,  #Assume first is correct
                             exercise = 1,
                             course = "default",
                             lesson = "ggplot",
                             instructor = "dima",
                             totalstudents = 25,
                             sampledstudents = 15,
                             attemptbuffer = 2,
                             maxsleep = 20){
  for(s in sample(seq(totalstudents), sampledstudents)) {
    for(a in seq(sample(length(answerpool) + attemptbuffer, 1))){ #attempts
      answer = sample(answerpool, 1)
      isCorrect = answer == answerpool[1]
      swirl:::Parse_create("adb_dima",
                           course = course,
                           lesson = lesson,
                           instructor = instructor,
                           student = digest(s),
                           exercise = exercise,
                           correct = isCorrect,
                           answer  = answer)
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

simulateStudents(answerpool = q2_ans, exercise = 2, maxsleep = 12)
