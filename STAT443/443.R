dat <- read.csv("C:/users/whuang67/downloads/ShanghaiPM20100101_20151231.csv")

library(ggplot2)

general_plot <- function(variable){
  ggplot() +
    geom_histogram(data = dat,
                   mapping = aes_string(x = variable),
                   bins = 50) +
    ggtitle(variable)
}

#### Responses
general_plot("PM_Jingan")
general_plot("PM_US.Post")
general_plot("log(PM_US.Post)")
general_plot("sqrt(PM_US.Post)")

general_plot("PM_Xuhui")

ggplot(data = dat,
       mapping = aes(x = No)) +
  geom_line(mapping = aes(y = PM_Jingan),
            color = "red",
            alpha = .5) +
  geom_line(mapping = aes(y = PM_US.Post),
            color = "blue",
            alpha = .5) +
  geom_line(mapping = aes(y = PM_Xuhui),
            color = "orange",
            alpha = .5)

#### Predictors
general_plot("DEWP")
general_plot("HUMI")
general_plot("PRES")
general_plot("TEMP")
general_plot("cbwd")
general_plot("Iws")
general_plot("log(Iws+1)")
general_plot("precipitation")
general_plot("log(precipitation+1)")
general_plot("Iprec")
general_plot("log(Iprec+1)")

table(dat$year)
colnames(dat)




##########################################################################
##########################################################################
scatter_plot <- function(variable){
  ggplot(data = dat,
         mapping = aes(y = PM_US.Post)) +
    geom_point(mapping = aes_string(x = variable),
               alpha = 0.1,
               size = 1)
}

scatter_plot("DEWP")
scatter_plot("HUMI")
scatter_plot("PRES")
scatter_plot("TEMP")
scatter_plot("cbwd")
scatter_plot("Iws")
scatter_plot("precipitation")
scatter_plot("Iprec")


##########################################################################
##########################################################################
get_RMSE <- function(actual, predicted){
  return(mean((actual-predicted)^2)^.5)
}
### remove more missing values here.
dat1 <- dat[is.na(dat$PM_US.Post) == FALSE, ]


### New variables

### Inpute
dat1$DEWP[is.na(dat1$DEWP)] <- mean(dat1$DEWP, na.rm=TRUE)
dat1$HUMI[is.na(dat1$HUMI)] <- mean(dat1$HUMI, na.rm=TRUE)
dat1$PRES[is.na(dat1$PRES)] <- mean(dat1$PRES, na.rm=TRUE)
dat1$TEMP[is.na(dat1$TEMP)] <- mean(dat1$TEMP, na.rm=TRUE)

dat1$cbwd <- as.character(dat1$cbwd)
dat1$cbwd[is.na(dat1$cbwd)] <- "NE"
dat1$cbwd <- as.factor(dat1$cbwd)

dat1$Iws[is.na(dat1$Iws)] <- mean(dat1$Iws, na.rm=TRUE)
dat1$precipitation[is.na(dat1$precipitation)] <- mean(dat1$precipitation, na.rm=TRUE)
dat1$Iprec[is.na(dat1$Iprec)] <- mean(dat1$Iprec, na.rm=TRUE)

## Linear Regression
model_1 <- lm(PM_US.Post ~ DEWP + HUMI + PRES + TEMP + Iws + as.factor(cbwd) +
                precipitation + Iprec + as.factor(month) + as.factor(hour) +
                as.factor(day), data = dat1)
par(mfrow = c(2,2))
plot(model_1)
library(MASS)
boxcox(model_1)

model_2 <- lm(log(PM_US.Post) ~ DEWP + HUMI + PRES + TEMP + Iws + as.factor(cbwd) +
                precipitation + Iprec + as.factor(month) + as.factor(hour) +
                as.factor(day), data = dat1)
plot(model_2)

model_3 <- lm(sqrt(PM_US.Post) ~ DEWP + HUMI + PRES + TEMP + Iws + as.factor(cbwd) +
                precipitation + Iprec + as.factor(month) + as.factor(hour) +
                as.factor(day), data = dat1)
plot(model_3)

model_4 <- lm((PM_US.Post)^.2 ~ DEWP + HUMI + PRES + TEMP + Iws + as.factor(cbwd) +
                precipitation + Iprec + as.factor(month) + as.factor(hour) +
                as.factor(day), data = dat1)
plot(model_4)

vif(model_1)
## Day is not significant
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

get_RMSE(dat1$PM_US.Post, predict(model_1, dat1))
get_RMSE(dat1$PM_US.Post, exp(predict(model_2, dat1)))
get_RMSE(dat1$PM_US.Post, (predict(model_3, dat1))^2)
get_RMSE(dat1$PM_US.Post, (predict(model_4, dat1))^5)

#####################################################################

model_11 <- lm(PM_US.Post ~ DEWP + HUMI + PRES + TEMP + log(Iws+1) + as.factor(cbwd) +
                log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
                as.factor(day), data = dat1)
par(mfrow = c(2,2))
plot(model_11)
boxcox(model_11)

model_12 <- lm(log(PM_US.Post) ~ DEWP + HUMI + PRES + TEMP + log(Iws+1) + as.factor(cbwd) +
                 log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
                 as.factor(day), data = dat1)
par(mfrow = c(2,2))
plot(model_12)

model_13 <- lm(sqrt(PM_US.Post) ~ DEWP + HUMI + PRES + TEMP + log(Iws+1) + as.factor(cbwd) +
                 log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
                 as.factor(day), data = dat1)
par(mfrow = c(2,2))
plot(model_13)

model_14 <- lm(PM_US.Post^.2 ~ DEWP + HUMI + PRES + TEMP + log(Iws+1) + as.factor(cbwd) +
                 log(precipitation+1) + log(Iprec+1) + as.factor(month) + as.factor(hour) +
                 as.factor(day), data = dat1)
par(mfrow = c(2,2))
plot(model_14)

summary(model_11)
summary(model_12)
summary(model_13)
summary(model_14)
get_RMSE(dat1$PM_US.Post, predict(model_11, dat1))
get_RMSE(dat1$PM_US.Post, exp(predict(model_12, dat1)))
get_RMSE(dat1$PM_US.Post, predict(model_13, dat1)^2)
get_RMSE(dat1$PM_US.Post, predict(model_14, dat1)^5)

###########################################################################
model_24 <- lm(PM_US.Post^.2 ~ DEWP + HUMI + PRES + TEMP + log(Iws+1) + as.factor(cbwd) +
                 log(Iprec+1) + as.factor(month) + as.factor(hour) +
                 as.factor(day), data = dat1)
par(mfrow = c(2,2))
plot(model_24)
summary(model_24)
get_RMSE(dat1$PM_US.Post, predict(model_14, dat1)^5)

############################################################################
ggplot(data = dat[!(dat$year %in% c(2010, 2011)), ]) +
  geom_boxplot(mapping = aes(x = as.factor(month),
                         y = PM_US.Post,
                         fill = as.factor(year))) +
  facet_grid(as.factor(year) ~.) +
  ylim(0, 500)
max(dat$PM_Jingan, na.rm = TRUE)
max(dat$PM_US.Post, na.rm = TRUE)
max(dat$PM_Xuhui, na.rm = TRUE)



library(rsconnect)
deployApp(appDir = "C:/users/whuang67/downloads/New folder/STAT443")
