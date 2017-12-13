### Read dataset
dat <- read.csv("C:/users/whuang67/downloads/ShanghaiPM20100101_20151231.csv")
library(ggplot2)
library(reshape2)

saveRDS(model_rf_Xuhui, file="C:/users/whuang67/downloads/model_rf_Xuhui.rds")
model_rf_Jingan <- readRDS(file="C:/users/whuang67/downloads/model_rf_Jingan.rds")

dat2 <- melt(dat[, names(dat) %in% c("month", "PM_Jingan", "PM_Xuhui", "PM_US.Post")],
             id.vars = "month")

ggplot(data = dat2)+
  geom_point(mapping = aes(x = month,
                           y = value)) +
  facet_grid(variable ~ .)

### Generate new variable "weekday"
dat$Date <- as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
dat$weekday <- as.factor(weekdays(dat$Date))
dat$y <- c(dat$PM_Xuhui, rep(NA, 24))[(1+24):(52584+24)]
# dat <- dat[complete.cases(dat), ]
### Useful functions
get_RMSE <- function(actual, predicted) return(mean((actual-predicted)^2)^.5)

get_Accuracy <- function(actual, predicted){
  contToCat <- function(x) return(ifelse(x<75, "Low", ifelse(x<150, "Medium", "High")))
  actual_cat <- sapply(actual, FUN=contToCat)
  predicted_cat <- sapply(predicted, FUN=contToCat)
  
  return(list(accuracy = mean(actual_cat == predicted_cat),
              confusionMatrix = table(actual_cat, predicted_cat)))
}

### remove more missing values here.
dat1 <- dat[(is.na(dat$PM_Xuhui) == FALSE) &
              (is.na(dat$y) == FALSE) &
              (is.na(dat$cbwd) == FALSE), ]
# dat1 <- dat[is.na(dat$PM_US.Post) == FALSE, ]

dat1$DEWP[is.na(dat1$DEWP)] <- median(dat1$DEWP, na.rm=TRUE)
dat1$HUMI[is.na(dat1$HUMI)] <- median(dat1$HUMI, na.rm=TRUE)
dat1$PRES[is.na(dat1$PRES)] <- median(dat1$PRES, na.rm=TRUE)
dat1$TEMP[is.na(dat1$TEMP)] <- median(dat1$TEMP, na.rm=TRUE)
# dat1$cbwd <- as.character(dat1$cbwd)
# dat1$cbwd[is.na(dat1$cbwd)] <- "NE"
# dat1$cbwd <- as.factor(dat1$cbwd)
dat1$Iws[is.na(dat1$Iws)] <- median(dat1$Iws, na.rm=TRUE)
dat1$precipitation[is.na(dat1$precipitation)] <- median(dat1$precipitation, na.rm=TRUE)
dat1$Iprec[is.na(dat1$Iprec)] <- median(dat1$Iprec, na.rm=TRUE)
dat1$month <- as.factor(dat1$month)
dat1$hour <- as.factor(dat1$hour)



### Train test split
set.seed(1)
idx <- sample(1:nrow(dat1), nrow(dat1)*0.8)
train_dat <- dat1[idx, ]; test_dat <- dat1[-idx, ]

set.seed(1)
model_1 <- lm(log(y+1) ~ log(PM_US.Post+1) + DEWP + HUMI + PRES + TEMP + log(Iws+1) + as.factor(cbwd) +
                log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
                weekday, data = train_dat)

library(faraway)

vif(model_1)

model_ <- lm(log(y+1) ~ log(PM_US.Post+1) + HUMI + PRES + log(Iws+1) + as.factor(cbwd) +
                log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
                weekday, data = train_dat)
vif(model_)
par(mfrow = c(2,2))
plot(model_)

get_RMSE(train_dat$y, exp(predict(model_, train_dat))-1)
get_RMSE(test_dat$y, exp(predict(model_, test_dat))-1)
get_Accuracy(train_dat$y, exp(predict(model_, train_dat))-1)
get_Accuracy(test_dat$y, exp(predict(model_, test_dat))-1)


summary(model_1)
par(mfrow = c(2,2))
plot(model_1)
get_RMSE(train_dat$y, exp(predict(model_1, train_dat))-1)
get_RMSE(test_dat$y, exp(predict(model_1, test_dat))-1)

model_2 <- step(model_1, direction = "both")
get_RMSE(train_dat$y, exp(predict(model_2, train_dat))-1)
get_RMSE(test_dat$y, exp(predict(model_2, test_dat))-1)
get_Accuracy(train_dat$y, exp(predict(model_2, train_dat))-1)
get_Accuracy(test_dat$y, exp(predict(model_2, test_dat))-1)


library(randomForest)
model_rf_Xuhui <- randomForest(y ~ PM_Xuhui + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                              month + hour + weekday, data = train_dat,
                            ntree = 500)
# No: Iprec, precipitation
# variableImportance <- data.frame(model_rf_US$importance)
# ggplot(data=variableImportance) +
#   geom_bar(mapping = aes(x=row.names(variableImportance),
#                          y=IncNodePurity),
#            stat = "identity")
# 100, 500 no difference
get_RMSE(train_dat$y, (predict(model_rf_US, train_dat)))
get_RMSE(test_dat$y, (predict(model_rf_US, test_dat)))
get_Accuracy(train_dat$y, (predict(model_rf_US, train_dat)))
get_Accuracy(test_dat$y, (predict(model_rf_US, test_dat)))
## 100: train 12.63167, test 28.43827
## 500: train 12.46093, test 28.45565


model_2 <- lm(y ~ PM_US.Post + DEWP + HUMI + PRES + TEMP + Iws + as.factor(cbwd) +
                as.factor(month) + as.factor(hour) +
                weekday, data = train_dat)
summary(model_2)
par(mfrow = c(2,2))
plot(model_2)
get_RMSE(train_dat$y, exp(predict(model_2, train_dat)-1))
get_RMSE(test_dat$y, exp(predict(model_2, test_dat)-1))

get_RMSE(train_dat$y, predict(model_2, train_dat))
get_RMSE(train_dat$y, predict(model_2, train_dat))




GradientDescent <- function(x, y, learning_rate=0.001, batch_size=25, steps=100){
  n = nrow(x); p = ncol(x)+1 ## minus 1 plus 1
  idx <- sample(1:n, n, replace=FALSE)
  
  repeat_ = floor(n/batch_size)
  # if(n %% batch_size == 0){
  #   repeat_ = n/batch_size
  # } else{
  #   repeat_ = floor(n/batch_size) + 1
  # }

  weights = rep(0, p)
  x <- cbind(Intercept = rep(1, n), x)
  for(i in 1:steps){
    train_dataset <- as.matrix(x)
    ### train_idx <- (batch_size*(i-1)+1): (batch_size*i)
    ### train_dataset <- as.matrix(x[train_idx, ])
    # print(weights)
    # print(train_dataset[1:4, ])
    # model <- lm(formula, data = train_dataset)
    # y_pred <- predict(model, newdata = test_dataset)
    y_pred <- as.vector(train_dataset %*% weights)
    # y_actual <- y[train_idx]
    y_actual <- y
    
    gradient <- -t(train_dataset) %*% (y_actual - y_pred)/n
    weights = weights - gradient*learning_rate
    print(weights)
    print(i)
  }
  
  MSE <- mean((y - as.vector(as.matrix(x) %*% weights))^2)
  
  return(list(weights = weights,
              RMSE = MSE**.5))
}





dat$Date <- as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
dat$weekday <- as.factor(weekdays(dat$Date))
dat$y <- c(rep(NA, 24), dat$PM_US.Post)[1:52584]
### Useful functions
get_RMSE <- function(actual, predicted) return(mean((actual-predicted)^2)^.5)

### remove more missing values here.
dat1 <- dat[(is.na(dat$PM_US.Post) == FALSE) &
              (is.na(dat$y) == FALSE) &
              (is.na(dat$cbwd) == FALSE), ]
dat1$DEWP[is.na(dat1$DEWP)] <- median(dat1$DEWP, na.rm=TRUE)
dat1$HUMI[is.na(dat1$HUMI)] <- median(dat1$HUMI, na.rm=TRUE)
dat1$PRES[is.na(dat1$PRES)] <- median(dat1$PRES, na.rm=TRUE)
dat1$TEMP[is.na(dat1$TEMP)] <- median(dat1$TEMP, na.rm=TRUE)
# dat1$cbwd <- as.character(dat1$cbwd)
# dat1$cbwd[is.na(dat1$cbwd)] <- "NE"
# dat1$cbwd <- as.factor(dat1$cbwd)
dat1$Iws[is.na(dat1$Iws)] <- median(dat1$Iws, na.rm=TRUE)
dat1$precipitation[is.na(dat1$precipitation)] <- median(dat1$precipitation, na.rm=TRUE)
dat1$Iprec[is.na(dat1$Iprec)] <- median(dat1$Iprec, na.rm=TRUE)
dat1$month <- as.factor(dat1$month)
dat1$hour <- as.factor(dat1$hour)

## train model
library(Matrix)
set.seed(1)
idx <- sample(1:nrow(dat1), nrow(dat1)*0.8)
train_dat <- dat1[idx, ]; test_dat <- dat1[-idx, ]
dat_Gradient <- sparse.model.matrix(y ~ PM_US.Post + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                      precipitation + Iprec + month + hour + weekday - 1, data = train_dat)
dat_Gradient = data.frame(as.matrix(dat_Gradient))
dat_Gradient$cbwdcv <- NULL
w = GradientDescent(dat_Gradient, train_dat$y, steps = 2000, learning_rate=0.0000019)
w

# mean((exp(w$y_actual) - exp(w$y_pred))^2)^.5


testdat_Gradient <- sparse.model.matrix(y ~ PM_US.Post + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                                          precipitation + Iprec + month + hour + weekday - 1,
                                        data = test_dat)
testdat_Gradient = data.frame(as.matrix(testdat_Gradient))
testdat_Gradient$cbwdcv <- NULL
test_dat1 <- as.matrix(cbind(Intercept=rep(1, nrow(testdat_Gradient)), testdat_Gradient))
## test error
(mean((test_dat$y - test_dat1 %*% w$weights)^2))^.5
