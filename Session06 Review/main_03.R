# Question 01

ExchangeRate <- read.csv("03/ExchangeRate.csv")
ExchangeRateBTC <- ExchangeRate[ExchangeRate$Cryptocurrency == "BTC",]
ExchangeRateETH <- ExchangeRate[ExchangeRate$Cryptocurrency == "ETH",]
ExchangeRateXRP <- ExchangeRate[ExchangeRate$Cryptocurrency == "XRP",]

# Question 02

ExchangeRateBTC <- ExchangeRateBTC[order(ExchangeRateBTC$Date, ExchangeRateBTC$Time),]
ExchangeRateETH <- ExchangeRateETH[order(ExchangeRateETH$Date, ExchangeRateETH$Time),]
ExchangeRateXRP <- ExchangeRateXRP[order(ExchangeRateXRP$Date, ExchangeRateXRP$Time),]

# Question 03

TargetETH <- ExchangeRateETH[ExchangeRateETH$Date == "16-Aug-18",]
sprintf("Ethereum 16 August Open Price: Rp. %s", head(TargetETH, 1)$Price)
sprintf("Ethereum 16 August Close Price: Rp. %s", tail(TargetETH, 1)$Price)

# Question 04

TargetBTC <- ExchangeRateBTC[ExchangeRateBTC$Date == "15-Aug-18",]
sprintf("Bitcoin 15 August Low Price: Rp. %s", min(TargetBTC$Price))
sprintf("Bitcoin 15 August High Price: Rp. %s", max(TargetBTC$Price))

# Question 05

TargetXRP <- ExchangeRateXRP[ExchangeRateXRP$Date == "18-Aug-18",]
TargetXRP$Price <- as.double(TargetXRP$Price) * 1000
AverageXRP <- format(mean(TargetXRP$Price), big.mark = ".", decimal.mark = ",", nsmall = 3)
sprintf("Ripple 18 August Average Price: Rp. %s", AverageXRP)
MedianXRP <- format(median(TargetXRP$Price), big.mark = ".", decimal.mark = ",", nsmall = 1)
sprintf("Ripple 18 August Median Price: Rp. %s", MedianXRP)
DeviationXRP <- format(sd(TargetXRP$Price), big.mark = ".", decimal.mark = ",", nsmall = 5)
sprintf("Ripple 18 August Standard Deviation: %s", DeviationXRP)

# Question 06

summary(TargetXRP)