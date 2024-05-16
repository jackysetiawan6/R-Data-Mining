# IMPORT CSV FILE
sales <- read.csv("SupermarketSales.csv", fileEncoding = "UTF-8-BOM")

# SELECT SPECIFIC ROWS
sales <- sales[, c("Invoice.ID", "Branch", "City", "Gender", "Unit.price", "Quantity")]
sales <- sales[sales$City == "Naypyitaw" | sales$City == "Yangon", ]

# GET TOTAL PRICE
total <- sales$Unit.price * sales$Quantity
sales$Total.price <- round(total, 2)

# MODIFY FEMALE TO F
gender <- sales$Gender
gender[gender == "Female"] <- "F"
gender[gender == "Male"] <- "M"
sales$Gender <- gender

# ORDER BY TOTAL PRICE
sales <- sales[order(sales$Total.price, decreasing = TRUE), ]

# SELECT TOP 10 ROWS
sales <- head(sales, 10)

# BARPLOT
barplot(sales$Total.price, names.arg = sales$Invoice.ID, las = 2, col = "blue",
        main = "Total Price", xlab = "Invoice ID", ylab = "Total Price", horiz = TRUE,
        xpd = FALSE)