# IMPORT CSV FILE
score <- read.csv("Score.csv", fileEncoding="UTF-8-BOM")
scoreWeight <- read.csv("ScoreWeight.csv", fileEncoding="UTF-8-BOM")

# SELECT SPECIFIC COURSE
course <- scoreWeight[scoreWeight$Course.Code %in% score$Course.Code, ]

# CREATE MATRICES
matrixA <- course[, -1]

# 