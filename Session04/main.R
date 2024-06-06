# APRIORI LIBRARY
library(arules)

dataset <- data.frame(
  TransactionID = c(
    1, 1,
    2, 2, 2,
    3, 3, 3, 3,
    4,
    5, 5,
    6, 6, 6,
    7, 7, 7, 7, 7
  ),
  Item = c(
    "Milk", "Bread",
    "Milk", "Cola", "Cracker",
    "Cola", "Bread", "Cracker", "IceCream",
    "Cola",
    "Bread", "IceCream",
    "Milk", "Cracker", "Cola",
    "Milk", "Bread", "Cola", "Cracker", "IceCream"
  )
)

# CONVERTING DATAFRAME INTO OBJECT OF CLASS TRANSACTIONS
transactions <- as(split(dataset$Item, dataset$TransactionID), "transactions")

# SAVE THE RULES INTO A CSV FILE
write(transactions, "Transactions.csv", sep = ',')

# FINDING FREQUENT ITEMSETS USING APRIORI ALGORITHM
transactions.rules <- apriori(transactions, parameter = list(target = "frequent itemsets", support = 0.5, confidence = 0.5))

# PRINTING THE FREQUENT ITEMSETS RELATION
rules <- ruleInduction(transactions.rules, transactions)

# SAVE THE FREQUENT ITEMSETS INTO A CSV FILE
system('cmd', input = 'fpgrowth -s5 -k, Transactions.csv -o FrequentItemset.csv')

# SAVE THE TRANSACTION RULES INTO A CSV FILE
system('cmd', input = 'fpgrowth -tr -s5 -c70 -k, Transactions.csv -o TransactionRules.csv')