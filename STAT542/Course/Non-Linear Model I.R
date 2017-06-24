
set.seed(578)
x.example <- rep(0, 200)
x.example[1] <- rnorm(1, mean = 0, sd = 1)
for (i in 2:200){
  if (x.example[i-1] < 0){
    x.example[i] <- (-1.5)*x.example[i-1] + rnorm(1, mean = 0, sd = 1)
  }
  else {
    x.example[i] <- 0.5*x.example[i-1] + rnorm(1, mean = 0, sd = 1)
  }
}

library(ggplot2)
qplot(
  x = 1:200,
  y = x.example,
  geom = c("line", "point"),
  xlab = "Time Index",
  ylab = "Observations"
) + 
  geom_abline(intercept=c(0,0), slope=0) +
  ggtitle("2-regime TAR(1) series")

unemployment <- read.csv("/home/whuang67/578/578 Presentation/.Ruserdata/whuang67/US civilian unemployment.csv", header=TRUE)
unempl.sea <- as.matrix(rep(0, nrow(unemployment)/3))
for(i in 1:nrow(unempl.sea)){
  unempl.sea[i] = (unemployment$Value[3*i-2] +unemployment$Value[3*i-1]+unemployment$Value[3*i])/3
}
qplot(
  x = c(1:nrow(unempl.sea)),
  y = unempl.sea,
  xlab = "Time Index",
  ylab = "Seasonal Unemployment Rate",
  geom = c("line", "point")
) + ggtitle("U.S. seasonal civilian unemployment rate")

```{r, warning = FALSE, message = FALSE}
y <- diff(ts(unempl.sea), 1) # First differenced series
library(nonlinearTseries)
nonlinearityTest(y, verbose=FALSE)$TarTest$test.statistic
nonlinearityTest(y, verbose=FALSE)$TarTest$p.value


library(tsDyn)
unemployment.setarFit <- setar(
  y,
  d = 1,
  steps = 1,
  thVar=lag(y, k = 0),
  ML=c(2,3,4,12),
  MH=c(2,3,12),
  th=0.1,
  include="none"
)
summary(unemployment.setarFit)$AIC


summary(unemployment.setarFit)$object



qplot(
  x = 1:length(diff(ts(unempl.sea), 1)),
  y = as.numeric(diff(ts(unempl.sea), 1)),
  geom = c("line", "point"),
  xlab = "Time",
  ylab = "First Differenced Series",
  colour = "Actual Values"
) + 
  geom_abline(intercept = c(0, 0),
              slope = 0) +
  geom_abline(intercept = c(0, 0.1),
              slope = 0,
              linetype = 2) +
  geom_point(
    mapping = aes(
      x = 13:length(diff(ts(unempl.sea), 1)),
      y = as.numeric(ts(unemployment.setarFit$fitted.values)),
      colour="Fitted Values"
    )
  ) +
  geom_line(mapping = aes(
    x = 13:length(diff(ts(unempl.sea), 1)),
    y = as.numeric(ts(unemployment.setarFit$fitted.values)),
    colour="Fitted Values")) +
  ggtitle("Predicted and Actual values")



FindParameters <- selectSETAR(
  y, d = 1, steps = 1,
  thDelay = 0,
  nthresh = 1,
  mL = 2,
  mH = 2,
  trace = FALSE,
  plot = FALSE
)


AICs <- data.frame(FindParameters$allTh)

for(i in 1:nrow(AICs)){
  if(AICs$mL[i] == 1){
    if(AICs$mH[i] == 1){
      AICs$Group[i] <- "mL=1, mH=1"
    }
    else{
      AICs$Group[i] <- "mL=1, mH=2"
    }
  }
  if(AICs$mL[i] == 2){
    if(AICs$mH[i] == 1){
      AICs$Group[i] <- "mL=2, mH=1"
    }
    else{
      AICs$Group[i] <- "mL=2, mH=2"
    }
  }
}

ggplot(
  data = AICs,
  mapping = aes(
    x = th,
    y = pooled.AIC,
    group = Group,
    colour = Group
  )
) + 
  geom_point() +
  geom_line() +
  ggtitle("Pooled.AIC vs Threshold Values")
```

### Example of SETAR

```{r, warning =FALSE}
FindParameters$bests
unemployment.setarFit2 <- setar(
  y,
  d = 1,
  steps = 1,
  thVar = lag(y, k = 0),
  ML = 1,
  MH = c(1, 2),
  th = FindParameters$th,
  include = "none"
)
summary(unemployment.setarFit2)$AIC



summary(unemployment.setarFit2)$object


qplot(
  x = 1:(length(diff(ts(unempl.sea), 1))),
  y = as.numeric(diff(ts(unempl.sea), 1)),
  geom = c("line", "point"),
  xlab = "Time",
  ylab = "First Differenced Series",
  colour = "Actual Values"
) +
  geom_abline(intercept = c(0, 0),
              slope = 0) +
  geom_abline(intercept = c(0, FindParameters$th),
              slope = 0,
              linetype = 2) +
  geom_abline(intercept = c(0, 0.1),
              slope = 0,
              linetype = "dotted") +
  geom_point(
    mapping = aes(
      x = 13:length(diff(ts(unempl.sea), 1)),
      y = as.numeric(ts(unemployment.setarFit$fitted.values)),
      colour="Fitted Values 1st Attempt"
    )
  ) +
  geom_line(mapping = aes(
    x = 13:length(diff(ts(unempl.sea), 1)),
    y = as.numeric(ts(unemployment.setarFit$fitted.values)),
    colour="Fitted Values 1st Attempt"
  )
  ) +
  geom_point(
    mapping = aes(
      x = 3:length(diff(ts(unempl.sea), 1)),
      y = as.numeric(ts(unemployment.setarFit2$fitted.values)),
      colour="Fitted Values 2nd Attempt"
    )
  ) +
  geom_line(mapping = aes(
    x = 3:length(diff(ts(unempl.sea), 1)),
    y = as.numeric(ts(unemployment.setarFit2$fitted.values)),
    colour="Fitted Values 2nd Attempt"
  )
  ) +
  ggtitle("Actual, 1st & 2nd Attempted Predicted Values")



FindParameters3 <- selectSETAR(
  y, d = 1, steps = 0,
  thDelay = 1,
  nthresh = 1,
  mL = 2,
  mH = 2,
  trace = FALSE,
  plot = FALSE
)


FindParameters3$bests
unemployment.setarFit3 <- setar(
  y, d = 1, steps = 0,
  thVar = lag(y, k = -1),
  ML = 1, MH = 1,
  th = FindParameters3$th,
  include = "none"
)
summary(unemployment.setarFit3)$AIC


summary(unemployment.setarFit3)$coef


qplot(
  x = 1:(length(diff(ts(unempl.sea), 1))),
  y = as.numeric(diff(ts(unempl.sea), 1)),
  geom = "line",
  xlab = "Time",
  ylab = "First Differenced Series",
  colour = "Actual Values (Solid)"
) +
  geom_line(
    mapping = aes(
      x = 1:(length(unemployment.setarFit3$fitted.values)),
      y = unemployment.setarFit3$fitted.values,
      colour = "Fitted Values (Dashed)"
    ),
    linetype = 2
  ) +
  geom_abline(
    intercept = c(0, 0),
    slope = 0,
    linetype = "dotted"
  ) +
  ggtitle("Making steps=0 is really not good")



library(rafalib)
library(tsDyn)
set.seed(578)
x.example <- rep(0, 200)
x.example[1] <- rnorm(1, mean = 0, sd = 1)
for (i in 2:200){
  if (x.example[i-1] < 0){
    x.example[i] <- (-1.5)*x.example[i-1] + rnorm(1, mean = 0, sd = 1)
  }
  else {
    x.example[i] <- 0.5*x.example[i-1] + rnorm(1, mean = 0, sd = 1)
  }
}
y<-lstar(x.example,mL=1,mH=1,thDelay=1)$fitted.values 



library(rafalib)
library(tsDyn)
mypar()
plot(1:200,x.example,col='blue',type="l",xlab="T",ylab="X")
lines(2:199,y,col="red")
legend("topleft",c("TAR","STAR"),col=c("blue","red"),lty=c(1,1))


library(ggplot2)
x.lstar <- seq(-4, 4, by = 0.1)
y.lstar1 <- (1 + exp(-x.lstar))^(-1)
y.lstar3 <- (1 + exp(-3*x.lstar))^(-1)
y.lstar10 <- (1 + exp(-10*x.lstar))^(-1)
qplot(
  x = x.lstar,
  y = y.lstar1,
  geom = "line",
  colour = "gamma = 1",
  xlab = "z_t - c",
  ylab = "G()") + 
  geom_line(mapping = aes(x = x.lstar,
                          y = y.lstar3,
                          colour = "gamma = 3")) +
  geom_line(mapping = aes(x = x.lstar,
                          y = y.lstar10,
                          colour = "gamma = 10")) +
  ggtitle("Logistic transition functions of varying values of psi")




library(ggplot2)
x.estar <- seq(-4, 4, by = 0.1)
y.estar1 <- (1 - exp(-x.estar^2))
y.estar3 <- (1 - exp(-3*(x.estar^2)))
y.estar10 <- (1 - exp(-10*(x.estar^2)))
qplot(
  x = x.estar,
  y = y.estar1,
  geom = "line",
  colour = "gamma = 1",
  xlab = "z_t - c",
  ylab = "G()") + 
  geom_line(mapping = aes(x = x.estar,
                          y = y.estar3,
                          colour = "psi = gamma")) +
  geom_line(mapping = aes(x = x.estar,
                          y = y.estar10,
                          colour = "gamma = 10")) +
  ggtitle("Exponential transition functions of varying values of psi")



MMM<-read.table("/home/whuang67/578/578 Presentation/.Ruserdata/whuang67/m-mmm.dat")
n<-nrow(MMM)
data<-MMM[,1]
Box_rmean<-c(rep(0,20))
Box_rvariance<-c(rep(0,20))
plot(1:n,MMM[,1],main="monthly simple stock returns for 3M company",ylab="stock return",xlab="Time",type="l")
abline(h=0)


library("exts")
plot(emp_acf(MMM[,1]))






library("exts")
plot(emp_pacf(MMM[,1]))


library("exts")
plot(emp_pacf(MMM[,1]^2))


Arch_residual<-MMM[,1]-0.013
Star_residual<-MMM[,1]-0.015
Arch_sigma<-c(Arch_residual[1]^2,Arch_residual[2]^2, rep(0,n-2))
Star_sigma<-c(Star_residual[1]^2,Star_residual[2]^2, rep(0,n-2))
for(i in 3:n)
{
  Arch_sigma[i] <- 0.003 + 0.088*(Arch_residual[i-1])^2 + 0.109*(Arch_residual[i-2])^2
  Star_sigma[i] <- (0.003 + 0.205*(Star_residual[i-1])^2 + 0.092*(Star_residual[i-2])^2)+
    (0.001 - 0.239*(Star_residual[i-1])^2)/(1+exp(-1000*Star_residual[i-1]))
}

Stanres_Arch<-Arch_residual/sqrt(Arch_sigma)
Stanres_Star<-Star_residual/sqrt(Star_sigma)
autoplot(emp_acf(Stanres_Star^2))


autoplot(emp_pacf(Stanres_Star^2))


Star_residual<-MMM[,1]-0.017
Star_sigma<-c(Star_residual[1]^2,Star_residual[2]^2, rep(0,n-2))
for(i in 3:n)
{
  
  Star_sigma[i] <- (0.002 + 0.265*(Star_residual[i-1])^2 + 0.141*(Star_residual[i-2])^2)+
    (0.002 - 0.314*(Star_residual[i-1])^2)/(1+exp(-1000*Star_residual[i-1]))
}
Stanres_Star<-Star_residual/sqrt(Star_sigma)
Box_mean<-c(rep(0,20))
Box_variance<-c(rep(0,20))
for(i in 1:20)
{
  Box_mean[i]<-Box.test(Stanres_Star, lag = i, type = "Ljung-Box", fitdf = 1 )$p.value
  Box_variance[i]<-Box.test(Stanres_Star^2, lag = i, type = "Ljung-Box", fitdf = 5 )$p.value
}
mypar(1,2)
plot(2:20,Box_mean[-1],xlab="Lag",ylab="p-value",main="Ljung-Box results for mean",type="b",ylim=c(0,0.8))
abline(h=0.05)
plot(6:20,Box_variance[-(1:5)],xlab="Lag",ylab="p-value",main="Ljung-Box results for variance",type="b")
abline(h=0.05)


plot(1:n,Stanres_Star,main="",ylab="standardized residuals",xlab="Time",type="l")
abline(h=0)

