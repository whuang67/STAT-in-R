# Remove anything in workspace
rm(list = ls())

# Packages used in finance
inst_pkgs = load_pkgs =  c("segmented","ggplot2","psych","ISLR",
                           "splines","timeDate","plyr","cluster",
                           "wesanderson","RColorBrewer","ggthemes",
                           "quantmod","FinTS","fGarch","tseries",
                           "PerformanceAnalytics","FitAR","TTR",
                           "fPortfolio")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Load all packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

# Get the data from the quantmod package
getSymbols(c("^GSPC","MSFT","XOM","GE","JNJ","INTC","WFC","JPM"))


# Transform the Data in a timeSeries class
sp500.ret = timeSeries(na.omit(ClCl(GSPC)))
msft.ret = timeSeries(na.omit(ClCl(MSFT)))
xom.ret = timeSeries(na.omit(ClCl(XOM)))
ge.ret = timeSeries(na.omit(ClCl(GE)))
jnj.ret = timeSeries(na.omit(ClCl(JNJ)))
intc.ret = timeSeries(na.omit(ClCl(INTC)))
wfc.ret = timeSeries(na.omit(ClCl(WFC)))
jpm.ret = timeSeries(na.omit(ClCl(JPM)))

par(mfrow = c(2,1), oma = c(4,3,0,0) + 1, mar = c(0,0,1,1) + 1)
cumulatedPlot(msft.ret, title = FALSE)
seriesPlot(msft.ret, title = FALSE)

par(mfrow = c(2,1), oma = c(4,3,0,0) + 1, mar = c(0,0,1,1) + 1)
cumulatedPlot(xom.ret, title = FALSE)
seriesPlot(xom.ret, title = FALSE)

par(mfrow = c(2,1), oma = c(4,3,0,0) + 1, mar = c(0,0,1,1) + 1)
cumulatedPlot(ge.ret, title = FALSE)
seriesPlot(ge.ret, title = FALSE)

par(mfrow = c(2,1), oma = c(4,3,0,0) + 1, mar = c(0,0,1,1) + 1)
cumulatedPlot(intc.ret, title = FALSE)
seriesPlot(intc.ret, title = FALSE)

par(mfrow = c(2,1), oma = c(4,3,0,0) + 1, mar = c(0,0,1,1) + 1)
cumulatedPlot(jpm.ret, title = FALSE)
seriesPlot(jpm.ret, title = FALSE)

par(mfrow = c(2,1), oma = c(4,3,0,0) + 1, mar = c(0,0,1,1) + 1)
cumulatedPlot(wfc.ret, title = FALSE)
seriesPlot(wfc.ret, title = FALSE)

# Plot the the return series
par(mfrow = c(2,1), oma = c(4,3,0,0) + 1, mar = c(0,0,1,1) + 1)
cumulatedPlot(jnj.ret, title = FALSE)
seriesPlot(jnj.ret, title = FALSE)

#  Histogram
par(mfrow = c(1,1))
histPlot(msft.ret)

#  Histogram
histPlot(xom.ret)

#  Histogram
histPlot(ge.ret)

#  Histogram
histPlot(jnj.ret)

#  Histogram
histPlot(intc.ret)

#  Histogram
histPlot(wfc.ret)

#  Histogram
histPlot(jpm.ret)


library(fPortfolio)
# Plot the basic statistics
mat = cbind(msft.ret,xom.ret,ge.ret,intc.ret, jnj.ret, wfc.ret, jpm.ret)
dimnames(mat)[[2]] = c("MSFT","XOM","GE","INTC","JNJ","WFC","JPM")
assetsBasicStatsPlot(mat, main = " ", cex = 0.9)


# Correlation Matrix 
assetsCorImagePlot(mat)


longFrontier <- portfolioFrontier(mat)
plot(longFrontier)

lppSpec <- portfolioSpec()
setNFrontierPoints(lppSpec) <- 25
longFrontier <- portfolioFrontier(mat, lppSpec)
tailoredFrontierPlot(object = longFrontier,
                     mText = "MV Portfolio - LongOnly Constraints",
                     risk = "Cov", xlim = c(0.008,0.033))


frontierPlot(object = longFrontier, pch = 19, cex = 0.5)
twoAssetsLines(object = longFrontier, col = "orange", lwd = 2)
frontier <- frontierPoints(object = longFrontier)
lines(frontier, col = "red", lwd = 2)

par(mfrow = c(2,1))
weightsPlot(longFrontier)
covRiskBudgetsPlot(longFrontier)

mat = 100*mat
# Defines the type of porfolio
ewSpec = portfolioSpec()
nAssets = ncol(mat)
# Creates the equal weigths 
setWeights(ewSpec) = rep(1/nAssets, times = nAssets)
# Creates the Portfolio
ewPortfolio = feasiblePortfolio(data = mat,
                                spec = ewSpec, 
                                constraints = "LongOnly")

print(ewPortfolio) 

par(mfrow = c(2,2))
col = divPalette(ncol(mat), "Spectral")
weightsPie(ewPortfolio, radius = 0.7, col = col) 
covRiskBudgetsPie(ewPortfolio, radius = 0.7, col = col)
weightedReturnsPie(ewPortfolio, radius = 0.7, col = col)

# Construct eff portfolio with same expected value as EWP
minriskSpec <- portfolioSpec()
targetReturn <- getTargetReturn(ewPortfolio@portfolio)["mean"]
setTargetReturn(minriskSpec) <- targetReturn
minriskPortfolio <- efficientPortfolio(
  data = mat,
  spec = minriskSpec,
  constraints = "LongOnly")
print(minriskPortfolio)

par(mfrow = c(2,2))
weightsPie(minriskPortfolio, radius = 0.7, col = col) 
covRiskBudgetsPie(minriskPortfolio, radius = 0.7, col = col)
weightedReturnsPie(minriskPortfolio, radius = 0.7, col = col)

# Specify the portfolio type
globminSpec = portfolioSpec()
globminPortfolio = minvariancePortfolio(data = mat,
                                        spec = globminSpec, 
                                        constraints = "LongOnly")

print(globminPortfolio)

par(mfrow = c(2,2))
weightsPie(globminPortfolio, radius = 0.7, col = col)
covRiskBudgetsPie(globminPortfolio, radius = 0.7, col = col)
weightedReturnsPie(globminPortfolio, radius = 0.7, col = col)



# Step 1: Create the DVI
dvi <- DVI(Cl(GSPC), smooth = 5)

# Step 2: Construct your trading rule 
# (Buy if DVI < 0.5, sell short if DVI > 0.5)
signal <- Lag(ifelse(dvi$e1 < 0.5, 1, -1))

# Step 3: Implement The trading rules/equity curve
return <- ROC(Cl(GSPC))*signal


# Step 4: Evaluate strategy performance
charts.PerformanceSummary(return)


# Creation of the tangency Portfolio (Or Maximum Sharpe Ratio)
mat = cbind(sp500.ret,msft.ret,jnj.ret, xom.ret)
dimnames(mat)[[2]] = c("SP500","MSFT","JNJ","XOM")

tgSpec = portfolioSpec()
# We set the risk free rate at 0%
setRiskFreeRate(tgSpec) = 0
tgPortfolio = tangencyPortfolio(data = mat,
spec = tgSpec, 
constraints = "LongOnly")

tgBacktest = portfolioBacktest()
pftformula = SP500 ~ MSFT + JNJ + XOM
tgPortfolio = portfolioBacktesting(formula = pftformula, data = mat,
spec = tgSpec, 
constraints =  "LongOnly",
backtest = tgBacktest, 
trace = FALSE)

Wheigths = round(100*tgPortfolio$weights,2)[1:12,]

setSmootherLambda(tgPortfolio$backtest) = "12m"
ptfSmoothPortfolios = portfolioSmoothing(object = tgPortfolio, 
trace = FALSE)

smoothWheigths = round(100*ptfSmoothPortfolios$smoothWeights,2)[1:12,]

backtestPlot(ptfSmoothPortfolios, cex = 0.6, font = 1, family = "mono")

netPerformance(ptfSmoothPortfolios)

