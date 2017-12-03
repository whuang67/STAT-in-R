### Read dataset
dat <- read.csv("C:/users/whuang67/downloads/ShanghaiPM20100101_20151231.csv")

### Generate new variable "weekday"
dat$Date <- as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
dat$weekday <- as.factor(weekdays(dat$Date))
dat$y <- c(rep(NA, 24), dat$PM_US.Post)[1:52584]

### Useful functions
get_RMSE <- function(actual, predicted) return(mean((actual-predicted)^2)^.5)

### remove more missing values here.
dat1 <- dat[(is.na(dat$PM_US.Post) == FALSE) &
              is.na(dat$y) == FALSE, ]
# dat1 <- dat[is.na(dat$PM_US.Post) == FALSE, ]

dat1$DEWP[is.na(dat1$DEWP)] <- median(dat1$DEWP, na.rm=TRUE)
dat1$HUMI[is.na(dat1$HUMI)] <- median(dat1$HUMI, na.rm=TRUE)
dat1$PRES[is.na(dat1$PRES)] <- median(dat1$PRES, na.rm=TRUE)
dat1$TEMP[is.na(dat1$TEMP)] <- median(dat1$TEMP, na.rm=TRUE)
dat1$cbwd <- as.character(dat1$cbwd)
dat1$cbwd[is.na(dat1$cbwd)] <- "NE"
dat1$cbwd <- as.factor(dat1$cbwd)
dat1$Iws[is.na(dat1$Iws)] <- median(dat1$Iws, na.rm=TRUE)
dat1$precipitation[is.na(dat1$precipitation)] <- median(dat1$precipitation, na.rm=TRUE)
dat1$Iprec[is.na(dat1$Iprec)] <- median(dat1$Iprec, na.rm=TRUE)
dat1$month <- as.factor(dat1$month)
dat1$hour <- as.factor(dat1$hour)

### See the distribution of response y
library(ggplot2)
ggplot(data = dat) + geom_histogram(mapping = aes(x = log(y+1)))

### Train test split
set.seed(1)
idx <- sample(1:nrow(dat1), nrow(dat1)*0.8)
train_dat <- dat1[idx, ]; test_dat <- dat1[-idx, ]

set.seed(1)
model_1 <- lm(log(y+1) ~ PM_US.Post + DEWP + HUMI + PRES + TEMP + Iws + as.factor(cbwd) +
                precipitation + Iprec + as.factor(month) + as.factor(hour) +
                weekday, data = train_dat)

summary(model_1)
par(mfrow = c(2,2))
plot(model_1)
get_RMSE(train_dat$y, exp(predict(model_1, train_dat)-1))
get_RMSE(test_dat$y, exp(predict(model_1, test_dat)-1))



library(randomForest)
model_rf <- randomForest(log(y+1) ~ PM_US.Post + DEWP + HUMI + PRES + TEMP + Iws + cbwd +
                           precipitation + Iprec + month + hour + weekday, data = train_dat,
                         inportance = TRUE)
variableImportance <- data.frame(model_rf$importance)
ggplot(data=variableImportance) +
  geom_bar(mapping = aes(x=row.names(variableImportance),
                         y=IncNodePurity),
           stat = "identity")

get_RMSE(train_dat$y, exp(predict(model_rf, train_dat)-1))
get_RMSE(test_dat$y, exp(predict(model_rf, test_dat)-1))




library(MASS)
boxcox(model_1)
