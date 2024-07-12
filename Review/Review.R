# Chapter 01

Score <- read.csv("Score.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
Score <- na.omit(Score)
Score <- Score[!duplicated(Score),]

ScoreWeight <- read.csv("ScoreWeight.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
ScoreWeight <- na.omit(ScoreWeight)
ScoreWeight <- ScoreWeight[!duplicated(ScoreWeight),]

MergedData <- merge(Score, ScoreWeight, by = "Course.Code")
MergedData <- MergedData[!duplicated(MergedData),]
MergedData <- MergedData[order(MergedData$Course.Code),]

Result <- Score[order(Score$Course.Code),]

TotalAssignment <- MergedData$Assignment.x * MergedData$Assignment.y
TotalMidExam <- MergedData$Mid.Exam.x * MergedData$Mid.Exam.y
TotalFinalExam <- MergedData$Final.Exam.x * MergedData$Final.Exam.y

Grading <- function(Value) {
  if (Value >= 90)
    return("A")
  else if (Value >= 85)
    return("A-")
  else if (Value >= 80)
    return("B+")
  else if (Value >= 75)
    return("B")
  else if (Value >= 70)
    return("B-")
  else if (Value >= 65)
    return("C")
  else if (Value >= 50)
    return("D")
  else
    return("E")
}

Result$Total <- TotalAssignment + TotalMidExam + TotalFinalExam
Result$Grade <- sapply(Result$Total, Grading)

write.csv(Result, file = "OutputResult.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Chapter 02

Participant <- read.csv("Participant.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
Participant <- na.omit(Participant)
Participant <- Participant[!duplicated(Participant),]

Question <- read.csv("Question.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
Question <- na.omit(Question)
Question <- Question[!duplicated(Question),]

Result <- read.csv("Result.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
Result <- na.omit(Result)
Result <- Result[!duplicated(Result),]

SelectedQuestion <- Question[Question$Question.Number == 2, "Question"]
SelectedQuestion <- gsub("masters ", "masters\n", SelectedQuestion)
SelectedResult <- aggregate(Participant.Number ~ Question.2, data = Result, FUN = length)
SelectedResult$Percentage <- SelectedResult$Participant.Number / sum(SelectedResult$Participant.Number) * 100
SelectedGraph <- pie(SelectedResult$Percentage, col = rainbow(5),
                     main = paste("Result Percentage of Question 2:\n", SelectedQuestion),
                     labels = round(SelectedResult$Percentage, 1))
SelectedLegend <- legend(x = 1, y = 1, SelectedResult$Question.2, fill = rainbow(5), cex = 0.5,
                         legend = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))

SelectedQuestion <- Question[Question$Question.Number == 6, "Question"]
SelectedQuestion <- gsub("is ", "is\n", SelectedQuestion)
SelectedResult <- merge(Participant, Result, by = "Participant.Number")
SelectedResult <- na.omit(SelectedResult)
SelectedResult <- aggregate(Participant.Number ~ Gender, data = SelectedResult, FUN = length)
SelectedResult$Percentage <- SelectedResult$Participant.Number / sum(SelectedResult$Participant.Number) * 100
SelectedGraph <- pie(SelectedResult$Percentage, col = rainbow(2),
                     main = paste("Result Percentage of Question 6 for Female Participants:\n", SelectedQuestion),
                     labels = round(SelectedResult$Percentage, 1))