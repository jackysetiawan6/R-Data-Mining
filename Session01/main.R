# IMPORT CSV FILE
score <- read.csv("Score.csv", fileEncoding = "UTF-8")
scoreWeight <- read.csv("ScoreWeight.csv", fileEncoding = "UTF-8")

# SELECT SPECIFIC ROWS
courseWeight <- scoreWeight[scoreWeight$Course.Code %in% score$Course.Code, ]

# SORTING DATA BY COURSE CODE
score <- score[order(score$Course.Code), ]
courseWeight <- courseWeight[order(courseWeight$Course.Code), ]

# CREATE MATRICES WITHOUT COURSE CODE
matrixA <- as.matrix(score[, -1])
matrixB <- as.matrix(courseWeight[, -c(1, 5)])

# MULTIPLY MATRICES
matrixC <- matrixA %*% t(matrixB)

# GET TOTAL SCORE FROM DIAGONAL MATRIX
total <- diag(matrixC)
total <- as.data.frame(total)
colnames(total) <- "Total"

# GET GRADE BASED ON TOTAL SCORE
bins <- c(0, 50, 65, 70, 75, 80, 85, 90, 100)
labels <- c("E", "D", "C", "B-", "B", "B+", "A-", "A")
grade <- cut(total$Total, breaks = bins, labels = labels, include.lowest = TRUE, right = FALSE)
grade <- as.data.frame(grade)
colnames(grade) <- "Grade"

# ADD TOTAL SCORE TO DATAFRAME
score$Total <- total$Total
score$Grade <- grade$Grade

# WRITE MODIFIED DATA INTO CSV FILE
file_name = "Result.csv"
write.csv(score, file = file_name, row.names = FALSE, fileEncoding = "UTF-8")

# PRINT AND CHECK THE RESULT
result = read.csv(file_name, fileEncoding = "UTF-8")
print(result)