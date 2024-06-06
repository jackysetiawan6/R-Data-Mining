ScoreData <- read.csv("01/Score.csv", fileEncoding = "UTF-8-BOM")
ScoreData <- na.omit(ScoreData)
ScoreData <- ScoreData[!duplicated(ScoreData),]
ScoreData <- ScoreData[order(ScoreData$Course.Code),]

ScoreWeightData <- read.csv("01/ScoreWeight.csv", fileEncoding = "UTF-8-BOM")
ScoreWeightData <- na.omit(ScoreWeightData)
ScoreWeightData <- ScoreWeightData[!duplicated(ScoreWeightData),]
ScoreWeightData <- ScoreWeightData[order(ScoreWeightData$Course.Code),]

MergedData <- merge(ScoreData, ScoreWeightData, by = "Course.Code")
MergedData <- MergedData[!duplicated(MergedData),]
MergedData <- MergedData[order(MergedData$Course.Code),]

ResultAssignment <- MergedData$Assignment.x * MergedData$Assignment.y
ResultMid.Exam <- MergedData$Mid.Exam.x * MergedData$Mid.Exam.y
ResultFinal.Exam <- MergedData$Final.Exam.x * MergedData$Final.Exam.y

Result <- ScoreData
Result$Course.Code <- MergedData$Course.Code
Result$Total <- ResultAssignment + ResultMid.Exam + ResultFinal.Exam
Result$Grade <- ""

Grade <- function(Score) {
  if (Score >= 90) {
    return("A")
  } else if (Score >= 85) {
    return("A-")
  } else if (Score >= 80) {
    return("B+")
  } else if (Score >= 75) {
    return("B")
  } else if (Score >= 70) {
    return("B-")
  } else if (Score >= 65) {
    return("C")
  } else if (Score >= 50) {
    return("D")
  } else if (Score >= 0) {
    return("E")
  }
}

Result$Grade <- sapply(Result$Total, Grade)

write.csv(Result, file = "01/Result.csv", row.names = FALSE, fileEncoding = "UTF-8")