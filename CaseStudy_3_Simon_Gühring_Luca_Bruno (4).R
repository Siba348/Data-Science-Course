library(tseries)
library(zoo)
library(moments)
library(quantmod)


##Download stock prices for at least 100 stocks for the year 2022.

SPShares <- read.table("C:/Users/guehrings/Downloads/SP500Ticker (1).csv", sep=";", header=TRUE)
SPTickers <- SPShares$Symbol
SPTickers

## Get 100 Stocks Tickers out of S&P
HundredTickers_SP = SPTickers[seq(5, length(SPTickers), by = 5)]
length(HundredTickers_SP)

##Get the Prices

getPrices <- function(TickerSymbols,start,end,type){
  NumberOfStocks <- length(TickerSymbols)
  prices <- get.hist.quote(TickerSymbols[1],start=start,end=end,quote=type)
  goodSymbols <- TickerSymbols[1]
  for (d in 2:NumberOfStocks) {
    tryCatch({
      P <- get.hist.quote(TickerSymbols[d],start=start,end=end,quote=type)
      prices <- cbind(prices,P) 
      goodSymbols <- c(goodSymbols,TickerSymbols[d])
    }, error=function(err) { print(paste("Download ERROR: ", TickerSymbols[d])) } )
  }
  prices <- data.frame(coredata(prices))
  colnames(prices) <- goodSymbols
  NumberOfGoodStocks <- dim(prices)[2]
  T <- dim(prices)[1]
  badSymbols <- rep(FALSE,NumberOfGoodStocks)
  for (d in 1:NumberOfGoodStocks) {
    if (is.na(prices[1,d]) || is.na(prices[T,d])) {
      badSymbols[d] <- TRUE
    } else {
      if ( sum(is.na(prices[,d]))>0) { 
        print(paste(goodSymbols[d]," NAs filled: ", sum(is.na(prices[,d]))))
        prices[,d]<-na.approx(prices[,d])
      } 
    }
  }
  if (sum(badSymbols)>0){
    prices <- prices[!badSymbols]
    print(paste("Removed due to NAs: ", goodSymbols[badSymbols]))
  }
  if (sum(is.na(prices))==0 ) {
    if (sum(prices == 0) > 0) {print("Check Zeros!")}
  } else {print("Check NAs and Zeros")}
  prices
}

HundredSP_Prices <- getPrices(HundredTickers_SP, start="2022-01-01", end="2022-12-31", type="Adj")

## Calculate the Returns

getReturns <- function(prices) {
  NumberOfStocks <- dim(prices)[2]
  T <- dim(prices)[1]
  returns <- c()
  for (ind in 1:NumberOfStocks) {
    returns <- cbind(returns, diff(log(prices[,ind])))
  }
  returns
}

HundredSP_Returns <- data.frame(getReturns(HundredSP_Prices))

names(HundredSP_Returns) <- names(HundredSP_Prices)


## Calculate the covariance matrix and the correlation matrix

Correlation_HundredSP_Returns <- cor(HundredSP_Returns)

Covariance_HundredSP_Returns <- cov(HundredSP_Returns)

## Find the pair of stocks with the highest correlation

Max_Position <- which(Correlation_HundredSP_Returns == max(Correlation_HundredSP_Returns[Correlation_HundredSP_Returns < 1]), arr.ind = TRUE)
Max_Position

HundredTickers_SP[30]
HundredTickers_SP[60]

Correlation_HundredSP_Returns[30,60]

## FANG and MRO is the pair of stocks with the highest correlation.

## CHECK!
FANG <- get.hist.quote("FANG", start="2022-01-01", end="2022-12-31", "Adj")
MRO <- get.hist.quote("MRO", start="2022-01-01", end="2022-12-31", "Adj")

Matrix <- cbind(diff(log(FANG)),diff(log(MRO)))
cor(Matrix)

##Find the pair of stocks with the lowest correlation
Min_Position <- which(Correlation_HundredSP_Returns == min(Correlation_HundredSP_Returns), arr.ind = TRUE)
Min_Position

HundredTickers_SP[95]
HundredTickers_SP[59]
Correlation_HundredSP_Returns[95,59]

##VTRS and LMT is the pair of stock with the lowest correlation

## Compare the stand alone Risk and return (Lowest correlation)
SD_VTRS <- sd(HundredSP_Returns[,95])
SD_LMT <- sd(HundredSP_Returns[,59])
SD_VTRS
SD_LMT

MeanRet_VTRS <- mean(HundredSP_Returns[,95])
MeanRet_LMT <- mean(HundredSP_Returns[,59])
MeanRet_VTRS
MeanRet_LMT

data.frame(Stock= c("VTRS", "LMT"), Volatility=c(SD_VTRS, SD_LMT), Mean=c(MeanRet_VTRS,MeanRet_LMT))

## Compare the stand alone Risk and return (Highest Correlation)
SD_FANG <- sd(HundredSP_Returns[,30])
SD_MRO <- sd(HundredSP_Returns[,60])
SD_FANG
SD_MRO

MeanRet_FANG <- mean(HundredSP_Returns[,30])
MeanRet_MRO <- mean(HundredSP_Returns[,60])
MeanRet_FANG
MeanRet_MRO

data.frame(Stock= c("FANG", "MRO"), Volatility=c(SD_FANG, SD_MRO), Mean=c(MeanRet_FANG,MeanRet_MRO))

##Use the first pair of stocks to built portfolios with different combinations of weights.

PortfolioReturns <- function(StockReturns, weights) {
  if (sum(weights)==1 && length(weights)==dim(StockReturns)[2]) {
    NumStocks <- dim(StockReturns)[2]
    Length <- dim(StockReturns)[1]
    P <- rep(0,Length)
    for (t in 1:Length) {
      for (d in 1:NumStocks) {
        P[t] <- P[t] + weights[d]*exp(sum(StockReturns[1:t,d]))	
      }
    }
    P <- c(1,P)
    diff(log(P))
  } else {print("Error: weights do not match")}
}

TwoReturns1 <- data.frame(HundredSP_Returns[30],HundredSP_Returns[60]) 

Means_Pair1 <- c(MeanRet_FANG,MeanRet_MRO)
Volas_Pair1 <- c(SD_FANG, SD_MRO)

sig1 <- c()
mu1 <- c()

for (i in 0:100){
  PRet <- PortfolioReturns(TwoReturns1,c(i/100,1-i/100))
  mu1 <- c(mu1,mean(PRet))
  sig1 <- c(sig1,sd(PRet))
}

plot(sig1,mu1)

##Where to find the Equal Weighted Portfolio?

Equal_Weighted <- PortfolioReturns(TwoReturns1, c(0.5,0.5))
points(sd(Equal_Weighted),mean(Equal_Weighted), col="blue", pch=19)
sd(Equal_Weighted)

##Find the Portfolio with the smallest Volatility

MinVola1 <- min(sig1)
MinVola1

MinVolaIndex1 <- which(sig1 == MinVola1)

points(sig1[MinVolaIndex1], mu1[MinVolaIndex1], col = "red", pch = 19)

##Check if the Minimum Volatility equals the Volatility of the stock FANG
MinVola1 == SD_FANG

weights_MinVola1 <- c((MinVolaIndex1 - 1)/100, 1 - (MinVolaIndex1 - 1)/100)
weights_MinVola1

##The Portfolio with the smallest volatility is therefore the following: 100% FANG, 0% MRO

##Repeat with the second pair of stocks

TwoReturns2 <- data.frame(HundredSP_Returns[95],HundredSP_Returns[59]) 

Means_Pair2 <- c(MeanRet_VTRS,MeanRet_LMT)
Volas_Pair2 <- c(SD_VTRS, SD_LMT)

sig2 <- c()
mu2 <- c()

for (i in 0:100){
  PRet <- PortfolioReturns(TwoReturns2,c(i/100,1-i/100))
  mu2 <- c(mu2,mean(PRet))
  sig2 <- c(sig2,sd(PRet))
}

plot(sig2,mu2)

##Where to find the Equal Weighted Portfolio?

Equal_Weighted2 <- PortfolioReturns(TwoReturns2, c(0.5,0.5))
points(sd(Equal_Weighted2),mean(Equal_Weighted2), col="blue", pch=19)
sd(Equal_Weighted2)

## Find the Portfolio with the smallest Volatility
MinVola2 <- min(sig2)
MinVola2
MinVolaIndex2 <- which(sig2 == MinVola2)

points(sig2[MinVolaIndex2], mu2[MinVolaIndex2], col = "red", pch = 19) 


weights_MinVola2 <- c((MinVolaIndex2 - 1)/100, 1 - (MinVolaIndex2 - 1)/100)
weights_MinVola2

##The portfolio with the smallest volatility is the one with the following weights: 64% LMT and 36% VTRS.

##Compare the results

data.frame( StockPair = c("LMT and VTRS", "FANG and MRO"),
            Weights = c(paste0(weights_MinVola2[2], " and ", weights_MinVola2[1]), paste0(weights_MinVola1[1], " and ", weights_MinVola1[2])))

data.frame(Case= c("Highest Correlation", "Lowest Correlation"),Volatility=c(MinVola1, MinVola2))


##The Minimum Volatility of the portfolio with the HIGHEST correlation is higher than
##the Minimum Volatility of the portfolio with the LOWEST correlation.
##If the correlation between a pair of stocks is high, it means that they tend to move together, and the benefits of diversification in terms of risk reduction are limited.
##And as diversification is limited as they tend to move together, it also chose 100% of one stock (FANG) and 0% of another (MRO).
##Conversely, if the correlation is low or negative, the assets exhibit more independent price movements, potentially leading to a portfolio with lower volatility.


##Now combine the four stocks and calculate mean return and volatility of an equal weighted portfolio.

Returns_Four_Stocks <- data.frame(HundredSP_Returns[30],HundredSP_Returns[60], HundredSP_Returns[95],HundredSP_Returns[59])
Returns_Four_Stocks

Portfolio_Returns <- PortfolioReturns(Returns_Four_Stocks, c(0.25,0.25,0.25,0.25))
Mean_Portfolio <- mean(Portfolio_Returns)
Mean_Portfolio
Volatility_Portfolio <- sd(Portfolio_Returns)
Volatility_Portfolio

##Compare with the portfolios above

Mean_of_Minimum_Volatility1 <- mu1[MinVolaIndex1]

Mean_of_Minimum_volatility2 <- mu2[MinVolaIndex2]

data.frame(Case= c("Highest Correlation Only", "Lowest Correlation Only", "Equal Weighted Portfolio"),Volatility=c(MinVola1, MinVola2, Volatility_Portfolio), Mean=c(Mean_of_Minimum_Volatility1,Mean_of_Minimum_volatility2,Mean_Portfolio))

##Equal Weighted Portfolio has a lower volatility (Risk) than the one with the Highest Correlation only and still a very good Mean Return, that is near to the Mean Return of Highest Correlation Only.
##In other words: The Equal Weighted Portfolio has the best Mean to Volatility Ratio 


## Try to minimize portfolio volatility for the portfolio of four stocks.

Means <- c()
Volas <- c()
for (i in 1:4){
  Means <- c(Means, mean(Returns_Four_Stocks[,i]))
  Volas <- c(Volas,sd(Returns_Four_Stocks[,i]))
}

plot(Volas,Means,col="red",xlim=c(0.013,0.035), pch=15)    
text(Volas, Means, labels = c("FANG", "MRO", "VTRS", "LMT"), pos = 4, col = "blue")

mus <- c()
sigmas <- c()


for (i in seq(0,1,0.1)){
  for (j in seq(0,1-i,0.1)){
    for (k in seq(0,1-i-j,0.1)){
      PRet <- PortfolioReturns(Returns_Four_Stocks,c(i,j,k,max(1-(i+j+k),0)))
      mus <- c(mus,mean(PRet)); sigmas <- c(sigmas,sd(PRet))    
    } } }

plot(sigmas,mus, xlim=c(0.013,0.035))
points(Volas,Means,col="red",pch=15)
text(Volas, Means, labels = c("FANG", "MRO", "VTRS", "LMT"), pos = 4, col = "blue")

library(quadprog)

FindWeightsForMu <- function(Mu,CovMat,Means){
  N <- dim(CovMat)[1]
  AMat <- cbind(rep(1,N),Means,diag(1,nrow=N))
  bVec = c(1,Mu,rep(0,N))
  result = solve.QP(2*CovMat,rep(0,N),AMat,bVec,2)
  result
}

COV <- cov(Returns_Four_Stocks)

FindWeightsForMu(Mean_Portfolio,COV,Means)

points(sqrt(FindWeightsForMu(Mean_Portfolio,COV,Means)$value),Mean_Portfolio,col="blue",pch=16,cex=1.5)

Solution <- FindWeightsForMu(Mean_Portfolio,COV,Means)
Solution$solution
##With the above weight, the variance is minimal.


###
### PART 2
##Calculate mean return and volatility for an equal weighted portfolio of all downloaded stocks. 

PortfolioRet_All_Stocks <- PortfolioReturns(HundredSP_Returns, rep(0.01,times=100))
Mean_Portfolio_All_Stocks <- mean(PortfolioRet_All_Stocks)
Mean_Portfolio_All_Stocks

PortfolioVola_All_Stocks <- sd(PortfolioRet_All_Stocks)
PortfolioVola_All_Stocks

##Compare with the results of part 1

data.frame(Case= c("Highest Corr Equal Weighted Portfolio", "Lowest Corr Equal Weighted Portfolio", "Portfolio of Four Stocks", "Portfolio of 100 Stocks"),Volatility=c(sd(Equal_Weighted) , sd(Equal_Weighted2),Volatility_Portfolio, PortfolioVola_All_Stocks), Mean=c(mean(Equal_Weighted), mean(Equal_Weighted2), Mean_Portfolio, Mean_Portfolio_All_Stocks))

##Volatility is lower with 100 Stocks as it is more diversified; Even more than the Equal weighted Portfolio with the two stocks with the lowest correlation


## As a general rule portfolio volatility of a large equal weighted portfolio is close to the root of the average covariance of its constituents
##Check this relation for the equal weighted portfolio of all downloaded stocks
COV <- cov(HundredSP_Returns)
COV

avg_covariance <- (mean(COV[upper.tri(COV)]))^(1/2)
avg_covariance

## The use of the upper.tri function is essential to extract the upper triangular part of the covariance matrix. This is necessary because the matrix contains duplicated variances and includes the diagonal line, which represents the covariance of one stock with itself.   

data.frame(Case= c("Portfolio Volatility", "Estimator Covariance"), Value= c(PortfolioVola_All_Stocks, avg_covariance))

##Repeat part 2 for the year 2021

SP_Prices2021 <- getPrices(HundredTickers_SP, start="2021-01-01", end="2021-12-31", type="Adj")
SP_Returns2021 <- getReturns(SP_Prices2021)

PortfolioRet2021 <- PortfolioReturns(SP_Returns2021, rep(0.01,times=100))
Mean_Portfolio2021 <- mean(PortfolioRet2021)
Mean_Portfolio2021

PortfolioVola2021 <- sd(PortfolioRet2021)
PortfolioVola2021

COV21 <- cov(SP_Returns2021)
COV21

avg_covariance21 <- (mean(COV21[upper.tri(COV21)]))^(1/2)
avg_covariance21

data.frame(Case= c("Portfolio Volatility", "Estimator Covariance"), Value= c(PortfolioVola2021, avg_covariance21))

##And for the year 2020

SP_Prices2020 <- getPrices(HundredTickers_SP, start="2020-01-01", end="2020-12-31", type="Adj")
SP_Returns2020 <- getReturns(SP_Prices2020)

PortfolioRet2020 <- PortfolioReturns(SP_Returns2020, rep(0.01,times=100))
Mean_Portfolio2020 <- mean(PortfolioRet2020)
Mean_Portfolio2020

PortfolioVola2020 <- sd(PortfolioRet2020)
PortfolioVola2020

COV20 <- cov(SP_Returns2020)
COV20

avg_covariance20 <- (mean(COV20[upper.tri(COV20)]))^(1/2)
avg_covariance20

data.frame(Case= c("Portfolio Volatility", "Estimator Covariance"), Value= c(PortfolioVola2020, avg_covariance20))

##As a general rule, one can say that the volatility of a large, equally weighted portfolio approximates the root of the average covariance of its constituents.

