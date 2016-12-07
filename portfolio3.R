inst_pkgs = load_pkgs = c("segmented","ggplot2","psych","ISLR",
                          "splines","timeDate","plyr","cluster",
                          "wesanderson","RColorBrewer","ggthemes",
                          "quantmod","FinTS","fGarch","tseries",
                          "PerformanceAnalytics","FitAR","TTR",
                          "fPortfolio")

inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

install.packages("Rsymphony", type = "source")
library(quantmod)
install.packages("fPortfolio")
install.packages("timeSeries")
library(fPortfolio)
install.packages("fBasics")
install.packages("rugarch")
install.packages("rmgarch")
library(MTS)
library(fBasics)
library(rugarch)
library(rmgarch)
library(xlsx)
library(timeSeries)

#data import
data<-read.xlsx2("/Users/ensakaishiro/Desktop/Student_data_Set-2.xlsx",1,
                 colIndex = c(2,3,6,9,12,15,18), 
                 colClasses=c("Date","numeric","numeric","numeric","numeric","numeric","numeric"))
data<-data[-c(1,2),]
colnames(data)<-c("Date","asset1","asset2","asset3","asset4","asset5","benchmark")
data<-timeSeries(data[,2:7],data[,1])



myEstimator2<-function(x,spec=NULL, ...)
{
  data<-x  
  n<-nrow(data)
  m<-ncol(data)
  data<-data*100
  data_matrix<-as.matrix(data)
  p<-VARorder(data_matrix)$aicor
  mean_model<-VAR(data_matrix,p)
  mean_predict<-VARpred(mean_model, h = 1)$pred
  data1<-mean_model$residuals
  
  #DCC
  assespec<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                         mean.model=list(include.mean=FALSE,armaOrder = c(0, 0)))
  
  H<-diag(
    sapply(
      as.data.frame(data1),function(x)ugarchforecast(ugarchfit(assespec, x),
                                                     n.ahead = 1)@forecast$sigmaFor)^(0.25))
  
  # asset variance from marginal garch
  mspec<-multispec(replicate(m, assespec))
  
  dccspec1<-rmgarch::dccspec(mspec,dccOrder = c(1,1), model =c("DCC"))
  
  dccmodel<-dccfit(dccspec1, data1)
  
  dccpredict<-dccforecast(dccmodel, n.ahead = 1)
  
  validity<-H%*%dccpredict@mforecast$R[[1]][,,1]%*%H
  
  Sigma = matrix(validity,m,m,dimnames=list(names(mean_predict),names(mean_predict)))
  
list(mu = mean_predict,Sigma = Sigma)
}
pluginEstimator2 <- function (x, spec = NULL, ...) 
{
  x.mat = as.matrix(x)
  list(mu = myEstimator2(x)[[1]], Sigma = myEstimator2(x)[[2]])}

## 1 step forecast based on whole training data
Spec <- portfolioSpec()
setEstimator(Spec) <- "pluginEstimator2"
tgPortfolio = tangencyPortfolio(data = data,
                                spec = Spec,
                                constraints = "LongOnly")

## back test on training data (may take along time)
tgBacktest = portfolioBacktest()
pftformula =  benchmark ~ asset1 + asset2 + asset3 + asset4+ asset5
tgPortfolio = portfolioBacktesting(formula = pftformula, data = data,
                                   spec = Spec,
                                   constraints = "LongOnly",
                                   backtest = tgBacktest)
                                   
