####################################################################################
## 1. The models that support Shiny app can be obtained by running lines 149-152. ##
## 2. In order to run Shiny locally, please put app.R, the csv file and these     ##
##    three models from 1. in the same folder.                                    ##
####################################################################################

dat <- read.csv("ShanghaiPM20100101_20151231.csv")

###### Useful functions ######
get_RMSE <- function(actual, predicted) return(mean((actual-predicted)^2)^.5)

get_Accuracy <- function(actual, predicted){
  contToCat <- function(x) return(ifelse(x<75, "Low", ifelse(x<150, "Medium", "High")))
  actual_cat <- sapply(actual, FUN=contToCat)
  predicted_cat <- sapply(predicted, FUN=contToCat)
  
  return(list(accuracy = mean(actual_cat == predicted_cat),
              confusionMatrix = table(actual_cat, predicted_cat)))
}


###### Preprocessing ######
preprocessing <- function(location){
  dat$Date <- as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
  dat$weekday <- as.factor(weekdays(dat$Date))
  dat$y <- c(dat[, names(dat)==location], rep(NA, 24))[(1+24):(52584+24)]
  
  dat1 <- dat[(is.na(dat[,names(dat)==location]) == FALSE) &
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
  
  set.seed(1)
  idx <- sample(1:nrow(dat1), nrow(dat1)*0.8)
  train_dat <- dat1[idx, ]; test_dat <- dat1[-idx, ]
  
  return(list(train_dat, test_dat))
}


###### Linear Regression ######
## US Post
dat_US <- preprocessing("PM_US.Post")
model_lm <- lm(log(y+1) ~ log(PM_US.Post+1) + DEWP + HUMI + PRES + TEMP + log(Iws+1) + as.factor(cbwd) +
                log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
                weekday, data = dat_US[[1]])
library(faraway)
vif(model_lm)
model_ <- lm(log(y+1) ~ log(PM_US.Post+1) + HUMI + PRES + log(Iws+1) + as.factor(cbwd) +
               log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
               weekday, data = dat_US[[2]])
vif(model_)
par(mfrow = c(2,2))
plot(model_)

get_RMSE(dat_US[[1]]$y, exp(predict(model_, dat_US[[1]]))-1)
get_RMSE(dat_US[[2]]$y, exp(predict(model_, dat_US[[2]]))-1)
get_Accuracy(dat_US[[1]]$y, exp(predict(model_, dat_US[[1]]))-1)
get_Accuracy(dat_US[[2]]$y, exp(predict(model_, dat_US[[2]]))-1)


## Jingna
dat_Jingan <- preprocessing("PM_Jingan")
model_1 <- lm(log(y+1) ~ log(PM_Jingan+1) + HUMI + PRES + log(Iws+1) + as.factor(cbwd) +
               log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
               weekday, data = dat_Jingan[[2]])
vif(model_1)
par(mfrow = c(2,2))
plot(model_1)

get_RMSE(dat_Jingan[[1]]$y, exp(predict(model_1, dat_Jingan[[1]]))-1)
get_RMSE(dat_Jingan[[2]]$y, exp(predict(model_1, dat_Jingan[[2]]))-1)
get_Accuracy(dat_Jingan[[1]]$y, exp(predict(model_1, dat_Jingan[[1]]))-1)
get_Accuracy(dat_Jingan[[2]]$y, exp(predict(model_1, dat_Jingan[[2]]))-1)


## Xuhui
dat_Xuhui <- preprocessing("PM_Xuhui")
model_2 <- lm(log(y+1) ~ log(PM_Xuhui+1) + HUMI + PRES + log(Iws+1) + as.factor(cbwd) +
                log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
                weekday, data = dat_Xuhui[[2]])
vif(model_2)
par(mfrow = c(2,2))
plot(model_2)

get_RMSE(dat_Xuhui[[1]]$y, exp(predict(model_2, dat_Xuhui[[1]]))-1)
get_RMSE(dat_Xuhui[[2]]$y, exp(predict(model_2, dat_Xuhui[[2]]))-1)
get_Accuracy(dat_Xuhui[[1]]$y, exp(predict(model_2, dat_Xuhui[[1]]))-1)
get_Accuracy(dat_Xuhui[[2]]$y, exp(predict(model_2, dat_Xuhui[[2]]))-1)



###### Random Forest ######
## variable Importance
library(randomForest)
# model_rf_Importance <- randomForest(y ~ PM_US.Post + Iprec + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
#                                  month + hour + weekday + precipitation, data = dat_US[[1]],
#                                ntree = 500,
#                                importance=TRUE)
# No: Iprec, precipitation
# variableImportance <- data.frame(model_rf_US$importance)
# ggplot(data=variableImportance) +
#   geom_bar(mapping = aes(x=row.names(variableImportance),
#                          y=IncNodePurity),
#            stat = "identity")


## US Post
model_rf_US <- randomForest(y ~ PM_US.Post + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                                 month + hour + weekday, data = dat_US[[1]],
                               ntree = 100)
get_RMSE(dat_US[[1]]$y, (predict(model_rf_US, dat_US[[1]])))
get_RMSE(dat_US[[2]]$y, (predict(model_rf_US, dat_US[[2]])))
get_Accuracy(dat_US[[1]]$y, (predict(model_rf_US, dat_US[[1]])))
get_Accuracy(dat_US[[2]]$y, (predict(model_rf_US, dat_US[[2]])))


## Jingan
model_rf_Jingan <- randomForest(y ~ PM_Jingan + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                              month + hour + weekday, data = dat_Jingan[[1]],
                            ntree = 100)
get_RMSE(dat_Jingan[[1]]$y, (predict(model_rf_Jingan, dat_Jingan[[1]])))
get_RMSE(dat_Jingan[[2]]$y, (predict(model_rf_Jingan, dat_Jingan[[2]])))
get_Accuracy(dat_Jingan[[1]]$y, (predict(model_rf_Jingan, dat_Jingan[[1]])))
get_Accuracy(dat_Jingan[[2]]$y, (predict(model_rf_Jingan, dat_Jingan[[2]])))


## Xuhui
model_rf_Xuhui <- randomForest(y ~ PM_Xuhui + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                              month + hour + weekday, data = dat_Xuhui[[1]],
                            ntree = 100)
get_RMSE(dat_Xuhui[[1]]$y, (predict(model_rf_Xuhui, dat_Xuhui[[1]])))
get_RMSE(dat_Xuhui[[2]]$y, (predict(model_rf_Xuhui, dat_Xuhui[[2]])))
get_Accuracy(dat_Xuhui[[1]]$y, (predict(model_rf_Xuhui, dat_Xuhui[[1]])))
get_Accuracy(dat_Xuhui[[2]]$y, (predict(model_rf_Xuhui, dat_Xuhui[[2]])))


## Output final model
saveRDS(model_rf_US, file="model_rf_US.rds")
saveRDS(model_rf_Jingan, file="model_rf_Jingan.rds")
saveRDS(model_rf_Xuhui, file="model_rf_Xuhui.rds")

###### Stochastic Gradient Descent ######
## Function
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
  
  RMSE <- (mean((y - as.vector(as.matrix(x) %*% weights))^2))^.5
  
  return(list(weights = weights,
              RMSE = RMSE))
}

## US Post
library(Matrix)
dat_Gradient_US <- sparse.model.matrix(
  y ~ PM_US.Post + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
    precipitation + Iprec + month + hour + weekday - 1, data = dat_US[[1]])
dat_Gradient_US = data.frame(as.matrix(dat_Gradient_US))
dat_Gradient_US$cbwdcv <- NULL
w = GradientDescent(dat_Gradient_US, dat_US[[1]]$y, steps = 2000, learning_rate=0.0000019)
w

testdat_Gradient_US <- sparse.model.matrix(y ~ PM_US.Post + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                                          precipitation + Iprec + month + hour + weekday - 1,
                                        data = dat_US[[2]])
testdat_Gradient_US = data.frame(as.matrix(testdat_Gradient_US))
testdat_Gradient_US$cbwdcv <- NULL
test_dat1 <- as.matrix(cbind(Intercept=rep(1, nrow(testdat_Gradient_US)), testdat_Gradient_US))
## test error
(mean((dat_US[[2]]$y - test_dat1 %*% w$weights)^2))^.5


## Jingan
library(Matrix)
dat_Gradient_Jingan <- sparse.model.matrix(
  y ~ PM_Jingan + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
    precipitation + Iprec + month + hour + weekday - 1, data = dat_Jingan[[1]])
dat_Gradient_Jingan = data.frame(as.matrix(dat_Gradient_Jingan))
dat_Gradient_Jingan$cbwdcv <- NULL
w = GradientDescent(dat_Gradient, dat_Jingan[[1]]$y, steps = 2000, learning_rate=0.0000019)
w

testdat_Gradient_Jingan <- sparse.model.matrix(y ~ PM_Jingan + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                                             precipitation + Iprec + month + hour + weekday - 1,
                                           data = dat_Jingan[[2]])
testdat_Gradient_Jingan = data.frame(as.matrix(testdat_Gradient_Jingan))
testdat_Gradient_Jingan$cbwdcv <- NULL
test_dat1 <- as.matrix(cbind(Intercept=rep(1, nrow(testdat_Gradient_Jingan)), testdat_Gradient_Jingan))
## test error
(mean((dat_Jingan[[2]]$y - test_dat1 %*% w$weights)^2))^.5


## Xuhui
library(Matrix)
dat_Gradient_Xuhui <- sparse.model.matrix(
  y ~ PM_Xuhui + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
    precipitation + Iprec + month + hour + weekday - 1, data = dat_Xuhui[[1]])
dat_Gradient_Xuhui = data.frame(as.matrix(dat_Gradient_Xuhui))
dat_Gradient_Xuhui$cbwdcv <- NULL
w = GradientDescent(dat_Gradient_Xuhui, dat_Xuhui[[1]]$y, steps = 2000, learning_rate=0.0000019)
w

testdat_Gradient_Xuhui <- sparse.model.matrix(y ~ PM_Xuhui + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                                             precipitation + Iprec + month + hour + weekday - 1,
                                           data = dat_Xuhui[[2]])
testdat_Gradient_Xuhui = data.frame(as.matrix(testdat_Gradient_Xuhui))
testdat_Gradient_Xuhui$cbwdcv <- NULL
test_dat1 <- as.matrix(cbind(Intercept=rep(1, nrow(testdat_Gradient_Xuhui)), testdat_Gradient_Xuhui))
## test error
(mean((dat_Xuhui[[2]]$y - test_dat1 %*% w$weights)^2))^.5
