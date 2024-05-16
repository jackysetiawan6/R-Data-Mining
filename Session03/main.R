# SETTING UP SCIENTIFIC NOTATION
options(scipen=999)

# IMPORT CSV FILE
scoreData = read.csv("Score.csv", fileEncoding = "UTF-8-BOM", na.strings = "")
courseData = read.csv("Course.csv", fileEncoding = "UTF-8-BOM")

# REMOVE NA VALUES
scoreData = na.omit(scoreData)

# FILTER OUT INVALID SCORES USING INDEX
scoreData = scoreData[scoreData$Score >= 0 & scoreData$Score <= 100, ]

# FILTER OUT INVALID SCORES USING SUBSET
scoreData = subset(scoreData, scoreData$Score >= 0 & scoreData$Score <= 100, )

# CHECK FOR DUPLICATES
scoreData = scoreData[!duplicated(scoreData), ]

# SORT DATA BY NAME
scoreData = scoreData[order(scoreData$Name), ]
courseData = courseData[order(courseData$Course.Code), ]

# MERGE DATA USING INNER JOIN
mergedData = merge(scoreData, courseData, by.x = "Course.Code", by.y = "Course.Code", all = FALSE)

# DATA INFORMATION
cat("Mean Score: ", mean(mergedData$Score), "\n")
cat("Median Score: ", median(mergedData$Score), "\n")
cat("Standard Deviation: ", sd(mergedData$Score), "\n")
cat("Sum Score: ", sum(mergedData$Score), "\n")
cat("Length Score: ", length(mergedData$Score), "\n")

# DATA SUMMARY
summary(mergedData)

# GROUP DATA BY NAME
studentAverage = aggregate(Score ~ Name, data = scoreData, FUN = mean)
studentAverage = studentAverage[order(-studentAverage$Score), ]

# SHOW TOP 3 STUDENT'S AVERAGE SCORE
cat("Top 3 Student's Average Score: \n")
head(studentAverage, 3)

# SELECT STUDENT WHO PASSED 65 FOR EACH COURSE
studentPass = subset(mergedData, mergedData$Score >= 65)
studentPass$Subject = paste(studentPass$Course.Code, studentPass$Course.Name, sep = " - ")
studentPass = aggregate(Score ~ Subject, data = studentPass, FUN = length)

# TOTAL INCOME PER COURSE FOR EACH CREDIT WORTH 500000
income = aggregate(Credit ~ Course.Code, data = mergedData, FUN = sum)
income$Income = income$Credit * 500000