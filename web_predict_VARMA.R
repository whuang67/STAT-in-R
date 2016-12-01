installed.packages("autograde")
install.packages("exts")

install.packages("cranlogs")
install.packages("astsa")
library(vars)
library(astsa)
library("exts")
library("autograde")
library("cranlogs")
install.packages("MTS")
library("MTS")
devtools::install_github("coatless/autograde")
today = Sys.Date() - 1
web_all = cran_downloads(package = c("stringr","ggplot2","Rcpp","dplyr"),
                         from = "2015-01-01", to = today)
split_data = split( web_all , f = web_all$package )
Rcpp_data = split_data$Rcpp
stringr_data = split_data$stringr
ggplot2_data = split_data$ggplot2
dplyr_data = split_data$dplyr
#discard missing data




r = Rcpp_data$count[-which(Rcpp_data$count==0)]
s = stringr_data$count[-which(stringr_data$count==0)]
g = ggplot2_data$count[-which(ggplot2_data$count==0)]
d = dplyr_data$count[-which(dplyr_data$count==0)]
vecdata<-cbind(r,g,s,d)
VARselect(y = vecdata, lag.max = 20, type ="both", season = 7)
model<-VAR(y=vecdata, p = 10, season=7)
pred = predict(model, n.ahead=1)

#residuals test
plot(emp_acf(resid(model)[,1]))
plot(emp_pacf(resid(model)[,1]))
bdsTest(resid(model)[,1],m=5)

plot(emp_acf((resid(model)[,3])^2))
plot(emp_pacf((resid(model)[,3])^2))

#ann to residuals
install.packages("neuralnet")
library(neuralnet)
#specify the embedding dimension
VARselect(y = resid(model)[,2], lag.max = 20, type ="both")
#ann
#data disposal
r_y<-resid(model)[,1][11:649]
g_y<-resid(model)[,2][11:649]
s_y<-resid(model)[,3][11:649]
d_y<-resid(model)[,4][11:649]
r_x<-cbind(resid(model)[,1][1:639],resid(model)[,1][2:640],resid(model)[,1][3:641],resid(model)[,1][4:642],
     resid(model)[,1][5:643],resid(model)[,1][6:644],resid(model)[,1][7:645],resid(model)[,1][8:646],
     resid(model)[,1][9:647],resid(model)[,1][10:648])
g_x<-cbind(resid(model)[,2][1:639],resid(model)[,2][2:640],resid(model)[,2][3:641],resid(model)[,2][4:642],
           resid(model)[,2][5:643],resid(model)[,2][6:644],resid(model)[,2][7:645],resid(model)[,2][8:646],
           resid(model)[,2][9:647],resid(model)[,2][10:648])
s_x<-cbind(resid(model)[,3][1:639],resid(model)[,3][2:640],resid(model)[,3][3:641],resid(model)[,3][4:642],
           resid(model)[,3][5:643],resid(model)[,3][6:644],resid(model)[,3][7:645],resid(model)[,3][8:646],
           resid(model)[,3][9:647],resid(model)[,3][10:648])
d_x<-cbind(resid(model)[,4][1:639],resid(model)[,4][2:640],resid(model)[,4][3:641],resid(model)[,4][4:642],
           resid(model)[,4][5:643],resid(model)[,4][6:644],resid(model)[,4][7:645],resid(model)[,4][8:646],
           resid(model)[,4][9:647],resid(model)[,4][10:648])

r_data<-scale(cbind(r_y,r_x))
colnames(r_data)=c("Y","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")
g_data<-cbind(g_y,g_x)
colnames(g_data)=c("Y","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")
s_data<-cbind(s_y,s_x)
colnames(s_data)=c("Y","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")
d_data<-cbind(d_y,d_x)
colnames(d_data)=c("Y","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")

r_nn<-neuralnet(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,r_data,hidden=10,err.fct = "sse",stepmax = 1e+06)
g_nn<-neuralnet(Y~.,g_data,hidden=2)
s_nn<-neuralnet(Y~.,s_data,hidden=2)
d_nn<-neuralnet(Y~.,d_data,hidden=2)
#residual predict
pre_resid_r<-compute(r_nn,t(as.matrix(resid(model)[,1][638:647])))$net.result

pre_resid_g<-predict(g_nn,resid(model)[,2][640:649],type="raw")
pre_resid_s<-predict(s_nn,resid(model)[,3][640:649],type="raw")
pre_resid_d<-predict(d_nn,resid(model)[,4][640:649],type="raw")


a<-sVARMACpp(vecdata,order=c(0,1,0), sorder=c(0,1,1), s=7)

VARorder(a$residuals)



plot(emp_acf(a$residuals[,1]))
plot(emp_pacf(a$residuals[,1]))


model = arima(log(d+1), c(20,1,1), seasonal = list(order = c(1, 1, 1), period = 7))
exp(predict(model)$pred[[1]])

acf(model$residuals,1000)
plot(emp_acf(model$residuals))
plot(emp_pacf(model$residuals))

#MSE of historical data

MSE<-c(rep(0,100))
for(i in 1:100)
{
  predict(model,1)
}

autograde::make_web_pred(Rcpp = 9475,
                         ggplot2 = 9992 ,
                         stringr = 8112,
                         dplyr =  5987)



