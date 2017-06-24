 ## Read Data
library(rJava)
require(xlsx)
library(quantmod)
wholeData <- as.matrix(
  read.xlsx2("C:/users/whuang67/downloads/Student_data_Set.xlsx",
             sheetName = "Sheet1",
             startRow = 2)
)

## Subset useful Data sets
Date <- as.Date(as.numeric(as.character(wholeData[-1, 1])), origin = "1899-12-30")


Asset1 <- data.frame(as.numeric(as.character(wholeData[-1, 2])))
names(Asset1) <- "Return"
row.names(Asset1) <- Date

Asset2 <- data.frame(as.numeric(as.character(wholeData[-1, 5])))
names(Asset2) <- "Return"
row.names(Asset2) <- Date

Asset3 <- data.frame(as.numeric(as.character(wholeData[-1, 8])))
names(Asset3) <- "Return"
row.names(Asset3) <- Date

Asset4 <- data.frame(as.numeric(as.character(wholeData[-1, 11])))
names(Asset4) <- "Return"
row.names(Asset4) <- Date

Asset5 <- data.frame(as.numeric(as.character(wholeData[-1, 14])))
names(Asset5) <- "Return"
row.names(Asset5) <- Date

BenchMark <- data.frame(as.numeric(as.character(wholeData[-1, 17])))
names(BenchMark) <- "Return"
row.names(BenchMark) <- Date




### Cumulative Plot
test <- timeSeries(Asset1)
names(test)

mat <- cbind(timeSeries(Asset1),
             timeSeries(Asset2),
             timeSeries(Asset3),
             timeSeries(Asset4),
             timeSeries(Asset5))
names(mat) <- c("Asset1",
                "Asset2",
                "Asset3",
                "Asset4",
                "Asset5")

library(fPortfolio)
library(timeSeries)
library(PerformanceAnalytics)
library(fGarch)
### No short selling

assetsBasicStatsPlot(mat, main = " ", cex=0.5)
assetsCorImagePlot(mat, abbreviate = 6)


mat <- 100*mat
# Defines the type of porfolio
ewSpec <- portfolioSpec()
nAssets <- ncol(mat)


# Creates the ### EQUAL ### weigths
setWeights(ewSpec) <- rep(1/nAssets, times = nAssets)
# Creates the Portfolio
ewPortfolio <- feasiblePortfolio(data = mat,
                                 spec = ewSpec,
                                 constraints = "LongOnly")
print(ewPortfolio)
par(mfrow = c(2,2))
col <- divPalette(ncol(mat), "Spectral")
weightsPie(ewPortfolio, radius = 0.7, col = col)
covRiskBudgetsPie(ewPortfolio, radius = 0.7, col = col)
weightedReturnsPie(ewPortfolio, radius = 0.7, col = col)

# GLOBAL Minimum Variance Portfolio
globminSpec = portfolioSpec()
globminPortfolio = minvariancePortfolio(data = mat,
                                        spec = globminSpec,
                                        constraints = "LongOnly")
print(globminPortfolio)
par(mfrow = c(2,2))
weightsPie(globminPortfolio, radius = 0.7, col = col)
covRiskBudgetsPie(globminPortfolio, radius = 0.7, col = col)
weightedReturnsPie(globminPortfolio, radius = 0.7, col = col)


netPerformance(globminPortfolio)

#### Efficient Frontier
longFrontier <- portfolioFrontier(mat)
frontierPlot(object = longFrontier, pch = 19, cex = 0.5)
twoAssetsLines(object = longFrontier, col = "orange", lwd = 2)
frontier <- frontierPoints(object = longFrontier)
lines(frontier, col = "red", lwd = 2)

# Creation of the tangency Portfolio (Or Maximum Sharpe Ratio)
mat2 <- cbind(timeSeries(Asset1),
             timeSeries(Asset2),
             timeSeries(Asset3),
             timeSeries(Asset4),
             timeSeries(Asset5),
             timeSeries(BenchMark))
names(mat2) <- c("Asset1",
                "Asset2",
                "Asset3",
                "Asset4",
                "Asset5",
                "BenchMark")


tgSpec = portfolioSpec()
# We set the risk free rate at 0%
setRiskFreeRate(tgSpec) = 0
tgPortfolio = tangencyPortfolio(data = mat2,
                                spec = tgSpec, 
                                constraints = "LongOnly")



tgBacktest = portfolioBacktest()
pftformula = BenchMark ~ Asset1 + Asset2 + Asset3 + Asset4 + Asset5
tgPortfolio = portfolioBacktesting(formula = pftformula,
                                   data = mat2,
                                   spec = tgSpec, 
                                   constraints =  "LongOnly",
                                   backtest = tgBacktest, 
                                   trace = FALSE)

tgPortfolio$weights


Weigths <- round(100*tgPortfolio$weights,2)[1:12,]

setSmootherLambda(tgPortfolio$backtest) <- "12m"
ptfSmoothPortfolios = portfolioSmoothing(object = tgPortfolio, 
                                         trace = FALSE)

smoothWeigths <- round(100*ptfSmoothPortfolios$smoothWeights,2)[1:12,]

backtestPlot(ptfSmoothPortfolios, cex = 0.6, font = 1, family = "mono")

netPerformance(ptfSmoothPortfolios)






