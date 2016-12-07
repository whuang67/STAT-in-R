# Packages used in finance
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
library(fPortfolio)

## read .xlsx

data = read.csv('/Users/zykkmac/Box Sync/STAT 578/Student_data_Set.csv', header = F)
library(xlsx)
wholeData <- as.matrix(
  read.xlsx2("C:/users/whuang67/downloads/Student_data_Set.xlsx",
             sheetName = "Sheet1",
             startRow = 2)
)


Date <- as.Date(as.numeric(as.character(wholeData[-1, 1])), origin = "1899-12-30")


asset1 <- data.frame(as.numeric(as.character(wholeData[-1, 2])))
names(asset1) <- "Return"
row.names(asset1) <- Date

asset2 <- data.frame(as.numeric(as.character(wholeData[-1, 5])))
names(asset2) <- "Return"
row.names(asset2) <- Date

asset3 <- data.frame(as.numeric(as.character(wholeData[-1, 8])))
names(asset3) <- "Return"
row.names(asset3) <- Date

asset4 <- data.frame(as.numeric(as.character(wholeData[-1, 11])))
names(asset4) <- "Return"
row.names(asset4) <- Date

asset5 <- data.frame(as.numeric(as.character(wholeData[-1, 14])))
names(asset5) <- "Return"
row.names(asset5) <- Date

bench <- data.frame(as.numeric(as.character(wholeData[-1, 17])))
names(bench) <- "Return"
row.names(bench) <- Date

library(timeSeries)


head(data)
dim(data)
asset1 = data[, 1:2]
asset2 = data[, 4:5]
asset3 = data[, 7:8]
asset4 = data[, 10:11]
asset5 = data[, 13:14]
bench = data[, 16:17]
colnames(asset1) = c('date', 'ret')
colnames(asset2) = c('date', 'ret')
colnames(asset3) = c('date', 'ret')
colnames(asset4) = c('date', 'ret')
colnames(asset5) = c('date', 'ret')
colnames(bench) = c('date', 'ret')
head(asset1)
asset1$date = as.Date(asset1$date, format = '%m/%d/%y')
asset2$date = as.Date(asset2$date, format = '%m/%d/%y')
asset3$date = as.Date(asset3$date, format = '%m/%d/%y')
asset4$date = as.Date(asset4$date, format = '%m/%d/%y')
asset5$date = as.Date(asset5$date, format = '%m/%d/%y')
bench$date = as.Date(bench$date, format = '%m/%d/%y')

asset1 = na.omit(asset1)
asset2 = na.omit(asset2)
asset3 = na.omit(asset3)
asset4 = na.omit(asset4)
asset5 = na.omit(asset5)
bench = na.omit(bench)

asset1.ret = timeSeries(asset1)
asset2.ret = timeSeries(asset2)
asset3.ret = timeSeries(asset3)
asset4.ret = timeSeries(asset4)
asset5.ret = timeSeries(asset5)
bench.ret = timeSeries(bench)

colnames(asset1.ret) = c('Asset1 return')
colnames(asset2.ret) = c('Asset2 return')
colnames(asset3.ret) = c('Asset3 return')
colnames(asset4.ret) = c('Asset4 return')
colnames(asset5.ret) = c('Asset5 return')
colnames(bench.ret) = c('bench return')

train = (2/3)*nrow(asset1.ret)
A1.train = asset1.ret[1:train, ]
A1.test = asset1.ret[train:nrow(asset1.ret), ]
head(A1.train)
dim(A1.train)
dim(A1.test)
A1.mean = mean(A1.train)
A1.cov = cov(A1.train)
A1.var = var(A1.train)



# Plot the the return series for Asset1
par(mfrow = c(2, 1))
library(fPortfolio)
cumulatedPlot(asset1.ret, title = FALSE)
seriesPlot(asset1.ret, title = FALSE)

# Plot the the return series for Asset2
cumulatedPlot(asset2.ret, title = FALSE)
seriesPlot(asset2.ret, title = FALSE)

# Plot the the return series for Asset3
cumulatedPlot(asset3.ret, title = FALSE)
seriesPlot(asset3.ret, title = FALSE)

# Plot the the return series for Asset4
cumulatedPlot(asset4.ret, title = FALSE)
seriesPlot(asset4.ret, title = FALSE)

# Plot the the return series for Asset5
cumulatedPlot(asset5.ret, title = FALSE)
seriesPlot(asset5.ret, title = FALSE)

# Plot the the return series for BenchMark
cumulatedPlot(bench.ret, title = FALSE)
seriesPlot(bench.ret, title = FALSE)

## histograms
par(mfrow = c(3, 2))
histPlot(asset1.ret)
histPlot(asset2.ret)
histPlot(asset3.ret)
histPlot(asset4.ret)
histPlot(asset5.ret)
histPlot(bench.ret)

library(fPortfolio)
# plot basic statistics
mat = cbind(asset1.ret, asset2.ret, asset3.ret, asset4.ret, asset5.ret)
dimnames(mat)[[2]] = c('A1', 'A2', 'A3', 'A4', 'A5')
vars = c('A1', 'A2', 'A3', 'A4', 'A5')
assetsBasicStatsPlot(mat, main = ' ', cex = 0.5)

install.packages("PerformanceAnalytics")
install.packages("fImport")
install.packages("quadprog")
library(quadprog)
library(PerformanceAnalytics)

require(fImport)
require(PerformanceAnalytics)
require(tseries)
require(stats)


# Calculate Anuualized Returns for year 2013
mat2013 = mat[(2343-520):(2343-260), ]
t = table.AnnualizedReturns(na.omit(mat)[,vars],Rf=0)
t
## A4 has he highest annual sharpe ratio

#Get the annualized return and StdDev for each series from the table
rA1 = t['Annualized Return',"A1"]
rA2 = t['Annualized Return',"A2"]
rA3 = t['Annualized Return','A3']
rA4 = t['Annualized Return',"A4"]
rA5 = t['Annualized Return','A5']
sA1 = t['Annualized Std Dev',"A1"]
sA2 = t['Annualized Std Dev',"A2"]
sA3 = t['Annualized Std Dev',"A3"]
sA4 = t['Annualized Std Dev',"A4"]
sA5 = t['Annualized Std Dev',"A5"]

#Check the correlations
corr = cor(mat[,vars],use="complete.obs")
corr

#portfolio.optim cannot have NA values in the time series, filter them out
m2 = removeNA(mat[,vars])
wA1 = NULL
wA2 = NULL
wA3 = NULL
wA4 = NULL
wA5 = NULL
er = NULL
eStd = NULL

#loop through finding the optimum portfolio for return levels between
#the minimum (rA4) and the max(rA2)
#
#portfolio.optim uses daily returns, so we have to adjust accordingly
for (i in seq((rA2+0.12),(rA4 - 0.001),length.out=100)){
    pm = 1+i
    pm = log(pm)/260
    opt = portfolio.optim(m2,pm=pm)
    er = c(er,exp(pm*260)-1)
    eStd = c(eStd,opt$ps*sqrt(260))
    wA1 = c(wA1,opt$pw[1])
    wA2 = c(wA2,opt$pw[2])
    wA3 = c(wA3,opt$pw[3])
    wA4 = c(wA4,opt$pw[4])
    wA5 = c(wA5,opt$pw[5])
}


#Plot the efficient frontier for the 3 assets.
plot (eStd^2,er,col='blue')

solution = data.frame(wA1,wA2,wA3,wA4,wA5,er,eStd)




## create a function to optimize portfolio
marketPortfolio = function(merg,rf,returnNames, weightNames,graph=FALSE){
    
    #create an empty data frame for the portfolio weights
    weights = data.frame(t(rep(NA,length(weightNames))))
    colnames(weights) = weightNames
    weights = weights[-1,]
    #Calculate Annualized Returns
    t = table.AnnualizedReturns(merg[,returnNames])
    
    #Range to optimize over
    if (max(t['Annualized Return',])<0.005) maxRet = 0.002 else maxRet = max(t['Annualized Return',]) - .005
    
    if (min(t['Annualized Return',]) < -0.005) minRet = 0.001 else minRet = min(t['Annualized Return',]) + .005
    
    if (maxRet < minRet){
        minRet_t =minRet 
        maxRet_t =maxRet
        minRet = maxRet_t
        maxRet = minRet_T
    }
    #portfolio.optim cannot have NA values in the time series, filter
    #them out
    
    m2 = removeNA(merg[,returnNames])
    
    er = NULL
    eStd = NULL
    #loop through finding the optimum portfolio for return 
    #levels between the range found above
    #
    #portfolio.optim uses daily returns, so we have to 
    #adjust accordingly
    for (i in seq(minRet,maxRet,length.out=100)){
        pm = 1+i
        pm = log(pm)/nrow(merg)

        opt = portfolio.optim(m2,pm=pm)
        er = c(er,exp(pm*nrow(merg)) - 1)
        eStd = c(eStd,opt$ps*sqrt(nrow(merg)))
        w = t(opt$pw)
        colnames(w) = weightNames
        weights = rbind(weights,w)
    }
    
    solution = weights
    solution$er = er
    solution$eStd = eStd
    
    #find the index values for the minimum Std and the max Er
    minIdx = which(solution$eStd == min(solution$eStd))
    maxIdx = which(solution$er == max(solution$er))
    
    #subset the results
    subset = solution[minIdx:maxIdx,c('er','eStd')]
    subset$nAbove = NA
    
    #for each value in the subset, count the number of points
    #that lay below a line drawn through the point and the
    #RF asset
    for (i in seq(1,maxIdx - minIdx + 1)){
        toFit = data.frame(er=rf,eStd=0)
        toFit = rbind(toFit,subset[i,c('er','eStd')])
        fit = lm(toFit$er ~ toFit$eStd)
        poly = polynomial(coef = fit$coefficients)
        toPred = subset
        colnames(toPred) = c('actEr','eStd')
        toPred$er = predict(poly,toPred[,'eStd'])
        toPred$diff = toPred$er - toPred$actEr
        subset[i,'nAbove'] = nrow(toPred[which(toPred$diff > 0),])
    }
    
    #get the point of tangency — where the number of points
    #below the line is maximized
    max = max(subset$nAbove)
    er = subset[which(subset$nAbove == max),'er']
    eStd = subset[which(subset$nAbove == max),'eStd']
    
    #index of the market portfolio
    idx = which(solution$er == er & solution$eStd == eStd)
    
    #Draw the line if requested
    if (graph){
        maxStd = max(solution$eStd) + .02
        maxRetg = max(solution$er) + .02
        plot(solution$eStd,
             solution$er,
             xlim=c(0,maxStd),
             ylim=c(0,maxRetg),
             ylab='Expected Yearly Return',
             xlab='Expected Yearly Std Dev',
             main='Efficient Frontier',
             col='red',
             type='l',
             lwd=2)
        abline(v=c(0), col='black', lty='dotted')
        abline(h=c(0), col ='black', lty='dotted')
        
        toFit = data.frame(er=rf,eStd=0)
        toFit = rbind(toFit,solution[idx,c('er','eStd')])
        fit = lm(toFit$er ~ toFit$eStd)
        abline(coef=fit$coefficients,col='blue',lwd=2)
    }
    
    #Return the market portfolio weights and eStd and eR
    out = solution[idx,]
    return (out)
}    


## Example:
install.packages('polynom')
require(polynom)

merged = na.omit(mat)
vars = c('A1', 'A2', 'A3', 'A4', 'A5')
vars2 = c('A1', 'A2', 'A3', 'A4', 'A5')



# Use the first 2/3 as previous data and calculated the return for the rest day by day.
ret = numeric(414)
port_matrix = matrix(0,nrow = 414, ncol = 5)
for (i in 1564:1977){
    print(i)
    mp =  marketPortfolio(merged[1:(i-1), ],.01,vars,vars2,graph=FALSE)

    if (is.null(mp)){
        ret[i - 1563] = t(as.matrix(rep(1/length(stocks),length(stocks))))
        port_matrix[i-1563] = c(0.2, 0.2, 0.2, 0.2, 0.2)
    } 
    else {
        temp = as.matrix(merged[i, ]) %*% t(mp[1:5])
        ret[i-1563] = temp[1]
        port_matrix[i-1563] = mp[1:5]
        
    }
}

     
     
     
     
     
     
     
     
