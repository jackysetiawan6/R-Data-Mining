Header <- read.csv("04/Header.csv", fileEncoding = "UTF-8-BOM", na.strings = "")
Header <- na.omit(Header)
Header <- Header[!duplicated(Header),]

Games <- read.csv("04/Games.csv", fileEncoding = "UTF-8-BOM", na.strings = "")
Games <- na.omit(Games)
Games <- Games[!duplicated(Games),]

Detail <- read.csv("04/Detail.csv", fileEncoding = "UTF-8-BOM", na.strings = "")
Detail <- na.omit(Detail)
Detail <- Detail[!duplicated(Detail),]

library(arules)

MergedData <- merge(Detail, Games, by = "GameId")
MergedData <- merge(MergedData, Header, by = "TransactionId")
MergedData <- MergedData[!duplicated(MergedData),]

MergedData$TransactionId <- as.factor(MergedData$TransactionId)
MergedData$TransactionId <- droplevels(MergedData$TransactionId)

Transactions <- split(MergedData$Name, MergedData$TransactionId)
Rules <- apriori(Transactions, parameter = list(support = 0.005, maxlen = 10, target = "frequent itemsets"))
RulesInduction <- ruleInduction(Rules, confidence = 0.15)

inspect(RulesInduction)
