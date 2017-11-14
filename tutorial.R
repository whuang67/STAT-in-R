# Install packages
install.packages("ElemStatLearn")

# Load package and dataset
library(ElemStatLearn)
dat <- SAheart

# Data visualization
hist(dat$sbp) ## Histogram
plot(dat$sbp, dat$tobacco)  ## Scatter plot

# Vectorized calulation
### X1: 1 if sbp>160; 0 otherwise
X1 <- ifelse(dat$sbp > 160, 1, 0)
### X2: max(tobacco, 10)
X2 <- sapply(dat$tobacco, FUN = function(x) max(c(x, 10)))
X2_2 <- ifelse(dat$tobacco > 10, dat$tobacco, 10)

# Linear Regression
x <- runif(1000, 0, 10)
y <- 10 + 5*x + rnorm(1000, 0, 1)
plot(x, y)
### Model Fitting
LinearModel <- lm(y ~ x)
### Summary Information
summary(LinearModel)
plot(x, y)
abline(a = LinearModel$coefficients[1], b = LinearModel$coefficients[2], col="red")


# Logistic Regression
plot(dat$tobacco, dat$chd)
### Model Fitting
LogisticModel <- glm(chd ~ tobacco, data = dat, family=binomial)
### Summary Information
summary(LogisticModel)
plot(dat$tobacco, dat$chd)
p <- 1/(1+exp(-(LogisticModel$coefficients[1] + dat$tobacco*LogisticModel$coefficients[2])))
plot(dat$tobacco, dat$chd)
curve(expr = 1/(1+exp(-(LogisticModel$coefficients[1] + x*LogisticModel$coefficients[2]))),
      from = min(dat$tobacco), to = max(dat$tobacco), add=TRUE)


