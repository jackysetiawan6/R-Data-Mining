Participant <- read.csv("02/Participant.csv", fileEncoding = "UTF-8-BOM")
Participant <- na.omit(Participant)
Participant <- Participant[!duplicated(Participant),]

Question <- read.csv("02/Question.csv", fileEncoding = "UTF-8-BOM")
Question <- na.omit(Question)
Question <- Question[!duplicated(Question),]

Result <- read.csv("02/Result.csv", fileEncoding = "UTF-8-BOM")
Result <- na.omit(Result)
Result <- Result[!duplicated(Result),]

# Question 01

SecondQuestion <- Question[Question$Question.Number == 2, "Question"]
SecondQuestion <- gsub("masters ", "masters\n", SecondQuestion)
SecondTitle <- paste("Result Percentage of Question 2:\n", SecondQuestion)
SecondResult <- data.frame(table(Result$Question.2))
SecondResult$Percentage <- round(SecondResult$Freq / sum(SecondResult$Freq) * 100, 1)
SecondGraph <- pie(SecondResult$Freq, main = SecondTitle, labels = SecondResult$Percentage, col = rainbow(nrow(SecondResult)))
SecondLegend <- legend('topright', legend = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"), fill = rainbow(nrow(SecondResult)), cex = 1)

# Question 02

SixthQuestion <- Question[Question$Question.Number == 6, "Question"]
SixthQuestion <- gsub("is ", "is\n", SixthQuestion)
SixthTitle <- paste("Result Percentage of Question 6 for Female Participants:\n", SixthQuestion)
SixthResult <- merge(Participant, Result, by = "Participant.Number")
SixthResult <- SixthResult[SixthResult$Gender == "Female",]
SixthResult <- data.frame(table(SixthResult$Question.6))
SixthResult$Percentage <- round(SixthResult$Freq / sum(SixthResult$Freq) * 100, 1)
SixthGraph <- pie(SixthResult$Freq, main = SixthTitle, labels = SixthResult$Percentage, col = rainbow(nrow(SixthResult)))
SixthLegend <- legend('topright', legend = c("False", "True"), fill = rainbow(nrow(SixthResult)), cex = 1)

# Question 03

FirstQuestion <- Question[Question$Question.Number == 1, "Question"]
FirstQuestion <- gsub("Bluejack ", "Bluejack\n", FirstQuestion)
FirstQuestion <- gsub("great ", "great\n", FirstQuestion)
FirstTitle <- paste("Result Percentage of Question 1:\n", FirstQuestion)
FirstResult <- merge(Participant, Result, by = "Participant.Number")
FirstTableMale <- table(FirstResult[FirstResult$Gender == "Male", "Question.1"])
FirstTableFemale <- table(FirstResult[FirstResult$Gender == "Female", "Question.1"])
FirstTable<- t(cbind(FirstTableFemale, FirstTableMale))
FirstTableCategories <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
FirstGraph <- barplot(FirstTable, col = c("red", "orange"), main = FirstTitle)
FirstLegend <- legend('topleft', legend = c("Female", "Male"), fill = c("red", "orange"), cex = 1)

# Question 04

FirstQuestion <- Question[Question$Question.Number == 1, "Question"]
FirstQuestion <- gsub("Bluejack ", "Bluejack\n", FirstQuestion)
FirstQuestion <- gsub("great ", "great\n", FirstQuestion)
FirstTitle <- paste("Relationship of Gender and Result of Question 1:\n", FirstQuestion)
FirstResult <- merge(Participant, Result, by = "Participant.Number")
FirstGraph <- boxplot(FirstResult$Question.1 ~ FirstResult$Gender, xlab = "Gender", ylab = "Question 1 Answer", names = c("Female", "Male"), col = c("red", "orange"), main = FirstTitle)

# Question 05

FourthQuestion <- Question[Question$Question.Number == 4, "Question"]
FourthQuestion <- gsub("polite, ", "polite,\n", FourthQuestion)
FourthTitle <- paste("Result Percentage of Question 4:\n", FourthQuestion)
FourthResult <- hist(Result$Question.4, breaks = 5, col = c("red", "orange", "yellow", "green"), xlab = "Question 4 Answer", ylab = "Frequency", main = FourthTitle, ylim = c(0, 50))

# Question 06

FirstPercentage <- data.frame(table(Result$Question.1))$Freq / nrow(Result) * 100
SecondPercentage <- data.frame(table(Result$Question.2))$Freq / nrow(Result) * 100
ThirdPercentage <- data.frame(table(Result$Question.3))$Freq / nrow(Result) * 100
FourthPercentage <- data.frame(table(Result$Question.4))$Freq / nrow(Result) * 100
FifthPercentage <- data.frame(table(Result$Question.5))$Freq / nrow(Result) * 100
AllPercentage <- cbind(FirstPercentage, SecondPercentage, ThirdPercentage, FourthPercentage, FifthPercentage)
AllGraph <- barplot(AllPercentage, col = rainbow(nrow(AllPercentage)), names.arg = c("Question 1", "Question 2", "Question 3", "Question 4", "Question 5"))
AllLegend <- legend('topright', legend = c("Answered 1", "Answered 2", "Answered 3", "Answered 4", "Answered 5"), fill = rainbow(nrow(AllPercentage)), cex = 0.7)

# Question 07

ResultTitle <- "Number of Participant from\n2018-07-14 to 2018-07-20"
ResultRange <- aggregate(Participant.Number ~ Date, Result, length)
ResultRange <- rbind(ResultRange, c("7/15/2018", 0))
ResultRange <- ResultRange[order(ResultRange$Date),]
ResultRange$Date <- as.Date(ResultRange$Date, format = "%m/%d/%Y")
ResultGraph <- plot(ResultRange$Participant.Number, type = "p", xlab = "Survey Date", ylab = "Number of Participant", main = ResultTitle, xaxt = "n")
axis(1, at = 1:7, labels = ResultRange$Date)
ResultGraph <- plot(ResultRange$Participant.Number, type = "l", xlab = "Survey Date", ylab = "Number of Participant", main = ResultTitle, xaxt = "n")
axis(1, at = 1:7, labels = ResultRange$Date)

# Question 08

ResultGraph <- plot(ResultRange$Participant.Number, type = "o", xlab = "Survey Date", ylab = "Number of Participant", main = ResultTitle, xaxt = "n")
axis(1, at = 1:7, labels = ResultRange$Date)
dev.copy(png, "02/result.png", width = 600, height = 400)
dev.off()
