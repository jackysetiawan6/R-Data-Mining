# Load the data
general_data <- read.csv("GeneralData.csv", encoding="UTF-8-BOM")
general_data <- general_data[!duplicated(general_data),]
na.omit(general_data)

# 1. Show top 3 monthly income for Job role manager
manager <- general_data[general_data$JobRole == "Manager",]
manager <- manager[!duplicated(manager$MonthlyIncome),]
manager <- manager[order(-manager$MonthlyIncome),]
top_manager <- head(manager, 3)
barplot(top_manager$MonthlyIncome,
        names.arg=top_manager$MonthlyIncome,
        main="Top 3 Monthly Income for Manager",
        col=rainbow(3))

# 2. Show frequency gender of general data
gender_male <- nrow(general_data[general_data$Gender == "Male", ])
gender_female <- nrow(general_data[general_data$Gender == "Female", ])
barplot(c(gender_male, gender_female),
        names.arg=c("Male", "Female"),
        main="Gender Frequency",
        col=rainbow(2))

# 3. Get the type level of the job, 1 for ease, 2 for medium, 3 for hard, then make pie chart
job_ease <- nrow(general_data[general_data$JobLevel == 1,])
job_medium <- nrow(general_data[general_data$JobLevel == 2,])
job_hard <- nrow(general_data[general_data$JobLevel == 3,])
pie(c(job_ease, job_medium, job_hard),
    labels=c("Ease", "Medium", "Hard"),
    main="Job Level Type")

# 4. Get the type of YearsAtCompany 1 - 3 as NewBie, 4 - 6 as Senior, 7 - 20 as veteran than make the bar plot for each type
newbie <- nrow(general_data[general_data$YearsAtCompany >= 1 & general_data$YearsAtCompany <= 3,])
senior <- nrow(general_data[general_data$YearsAtCompany >= 4 & general_data$YearsAtCompany <= 6,])
veteran <- nrow(general_data[general_data$YearsAtCompany >= 7 & general_data$YearsAtCompany <= 20,])
barplot(c(newbie, senior, veteran),
        names.arg=c("Newbie", "Senior", "Veteran"),
        main="Years At Company Type",
        col=rainbow(3))

# 5.	Make apriori for JobRole that has work life balance, with support 0.005 and confidence 0.15
library(arules)

general_data <- read.csv("GeneralData.csv", encoding="UTF-8-BOM")
employee_survey_data <- read.csv("EmployeeSurveyData.csv", encoding="UTF-8-BOM")

employee_survey_data <- employee_survey_data[!duplicated(employee_survey_data),]
merged_data <- merge(general_data, employee_survey_data, by="EmployeeID")
merged_data <- merged_data[, c("EmployeeID", "JobRole", "WorkLifeBalance")]

merged_data$EmployeeID <- droplevels(as.factor(merged_data$EmployeeID))
merged_data$JobRole <- droplevels(as.factor(merged_data$JobRole))
merged_data$WorkLifeBalance <- droplevels(as.factor(merged_data$WorkLifeBalance))

work_life_balance <- split(merged_data$JobRole, merged_data$WorkLifeBalance)
rules <- apriori(work_life_balance, parameter=list(support=0.005, maxlen=10, target="frequent itemsets"))

inspect(rules)
inspect(ruleInduction(rules, confidence=0.15))