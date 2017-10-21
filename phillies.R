library(ggplot2)
library(gridExtra)
library(MASS)
library(ISLR)
library(caret)
library(randomForest)


dat <- read.csv("slugging.csv")

from_percentage_to_numeric <- function(vector){
  return(as.numeric(gsub("%", "", vector))/100)
}

dat$MarJun_BB. <- from_percentage_to_numeric(dat$MarJun_BB.)
dat$MarJun_K. <- from_percentage_to_numeric(dat$MarJun_K.)
dat$MarJun_LD. <- from_percentage_to_numeric(dat$MarJun_LD.)
dat$MarJun_GB. <- from_percentage_to_numeric(dat$MarJun_GB.)
dat$MarJun_FB. <- from_percentage_to_numeric(dat$MarJun_FB.)
dat$MarJun_HR.FB <- from_percentage_to_numeric(dat$MarJun_HR.FB)


visualization <- function(variable, univariate = TRUE){
  if(univariate == TRUE){
    mean = mean(dat[, names(dat) == variable])
    std = sd(dat[, names(dat) == variable])
    ggplot(data = dat) +
      geom_histogram(mapping = aes_string(x = variable,
                                          y = "..density.."),
                     bins = 30,
                     color = "white") +
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = std),
                    color = "red") +
      ggtitle(paste0("Histogram of ", variable))
  } else{
    
    y = dat$FullSeason_SLG
    x = dat[, names(dat) == variable]
    model = lm(y~x)
    ggplot(data = dat,
           mapping = aes(y = FullSeason_SLG)) +
      geom_point(mapping = aes_string(x = variable)) +
      geom_abline(slope = coef(model)[2],
                  intercept = coef(model)[1],
                  color = "red") +
      ggtitle(paste0("FullSeason_SLF vs ", variable))
  }
}

grid.arrange(visualization("MarJun_PA"), visualization("MarJun_AB"),
             visualization("MarJun_AVG"), visualization("MarJun_SLG"),
             ncol=2, nrow=2)

grid.arrange(visualization("MarJun_BB."), visualization("MarJun_K."),
             visualization("MarJun_BABIP"), visualization("MarJun_GB.FB"),
             ncol=2, nrow=2)

grid.arrange(visualization("MarJun_LD."), visualization("MarJun_GB."),
             visualization("MarJun_FB."), visualization("MarJun_HR.FB"),
             ncol=2, nrow=2)

# visualization("FullSeason_SLG")

visualization("FullSeason_SLG")


grid.arrange(visualization("MarJun_PA", univariate = FALSE),
             visualization("MarJun_AB", univariate = FALSE),
             visualization("MarJun_AVG", univariate = FALSE),
             visualization("MarJun_SLG", univariate = FALSE),
             ncol=2, nrow=2)
grid.arrange(visualization("MarJun_BB.", univariate = FALSE),
             visualization("MarJun_K.", univariate = FALSE),
             visualization("MarJun_BABIP", univariate = FALSE),
             visualization("MarJun_GB.FB", univariate = FALSE),
             ncol=2, nrow=2)

grid.arrange(visualization("MarJun_LD.", univariate = FALSE),
             visualization("MarJun_GB.", univariate = FALSE),
             visualization("MarJun_FB.", univariate = FALSE),
             visualization("MarJun_HR.FB", univariate = FALSE),
             ncol=2, nrow=2)


dat1 <- data.frame(scale(dat[, -1]))
dat1$FullSeason_SLG <- dat$FullSeason_SLG


set.seed(1)
idx <- sample(1:nrow(dat1), 0.8*nrow(dat1))
dat1_train <- dat1[idx, ]
dat1_test <- dat1[-idx, ]

get_RMSE <- function(actual, predicted){
  return(sqrt(mean((actual - predicted)^2)))
}



model_1 <- lm(FullSeason_SLG ~. , data= dat1_train)
summary(model_1)
par(mfrow = c(2, 2))
plot(model_1)
# get_RMSE(dat1_train$FullSeason_SLG, predict(model_1))
# get_RMSE(dat1_test$FullSeason_SLG, predict(model_1, newdata = dat1_test))



model_2 <- step(model_1, direction = "both")
summary(model_2)
par(mfrow = c(2, 2))
plot(model_2)
# get_RMSE(dat1_train$FullSeason_SLG, predict(model_2))
# get_RMSE(dat1_test$FullSeason_SLG, predict(model_2, newdata = dat1_test))




boxcox(model_2)


model_3 <- lm(log(FullSeason_SLG) ~ MarJun_PA + MarJun_AB + MarJun_AVG + MarJun_SLG +
                MarJun_BB. + MarJun_K. + MarJun_BABIP + MarJun_GB.FB + MarJun_LD. + 
                MarJun_GB. + MarJun_FB. + MarJun_HR.FB, data= dat1_train)
summary(model_3)
par(mfrow = c(2, 2))
plot(model_3)
# get_RMSE(dat1_train$FullSeason_SLG, exp(predict(model_3)))
# get_RMSE(dat1_test$FullSeason_SLG, exp(predict(model_3, newdata = dat1_test)))



set.seed(1)
ctrl <- trainControl(method = "cv")
grid <- expand.grid(k = seq(3, 29, 2))
dat$Name <- NULL
model_5 <- train(log(FullSeason_SLG) ~MarJun_PA + MarJun_AB + MarJun_AVG + MarJun_SLG +
                   MarJun_BB. + MarJun_K. + MarJun_BABIP + MarJun_GB.FB + MarJun_LD. + 
                   MarJun_GB. + MarJun_FB. + MarJun_HR.FB,
                 data = dat1_train,
                 method = "knn",
                 trControl = ctrl,
                 tuneGrid = grid)
print(model_5)

# get_RMSE(dat1_train$FullSeason_SLG, exp(predict(model_5)))
# get_RMSE(dat1_test$FullSeason_SLG, exp(predict(model_5, newdata = dat1_test)))



grid_RF <- expand.grid(mtry = 1:12)

model_6 <- train(log(FullSeason_SLG) ~MarJun_PA + MarJun_AB + MarJun_AVG + MarJun_SLG +
                   MarJun_BB. + MarJun_K. + MarJun_BABIP + MarJun_GB.FB + MarJun_LD. + 
                   MarJun_GB. + MarJun_FB. + MarJun_HR.FB,
                 data = dat1_train,
                 method = "rf",
                 ntree = 200,
                 trControl = ctrl,
                 tuneGrid = grid_RF)
print(model_6)
# get_RMSE(dat1_train$FullSeason_SLG, exp(predict(model_6)))
# get_RMSE(dat1_test$FullSeason_SLG, exp(predict(model_6, newdata = dat1_test)))



dat2 <- expand.grid(method = c("OLS", "OLS_step", "OLS_boxcox", "KNN", "RF"),
                    dataset = c("train", "test"))
dat2$RMSE <- c(get_RMSE(dat1_train$FullSeason_SLG, predict(model_1)),
               get_RMSE(dat1_train$FullSeason_SLG, predict(model_2)),
               get_RMSE(dat1_train$FullSeason_SLG, exp(predict(model_3))),
               get_RMSE(dat1_train$FullSeason_SLG, exp(predict(model_5))),
               get_RMSE(dat1_train$FullSeason_SLG, exp(predict(model_6))),
               get_RMSE(dat1_test$FullSeason_SLG, predict(model_1, newdata = dat1_test)),
               get_RMSE(dat1_test$FullSeason_SLG, predict(model_2, newdata = dat1_test)),
               get_RMSE(dat1_test$FullSeason_SLG, exp(predict(model_3, newdata = dat1_test))),
               get_RMSE(dat1_test$FullSeason_SLG, exp(predict(model_5, newdata = dat1_test))),
               get_RMSE(dat1_test$FullSeason_SLG, exp(predict(model_6, newdata = dat1_test))))

ggplot(data = dat2) +
  geom_bar(mapping = aes(x = method,
                         y = RMSE,
                         fill = dataset),
           stat = "identity",
           position = "dodge") +
  ggtitle("Summary")

