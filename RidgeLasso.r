# STAT / CES 542, Fall 2016
# This is the R code for lecture note Penalized
# This file includes model fitting of ridge regression

# MASS package is needed for lm.ridge

library(MASS)
set.seed(1)
n = 30
# highly correlated variables
X = mvrnorm(n, c(0, 0), matrix(c(1,0.999, 0.999, 1), 2,2))
y = rnorm(n, mean=1 + X[,1] + X[,2])
# compare parameter estimates
summary(lm(y~X))$coef
lm.ridge(y~X, lambda=5)

# the lm.ridge will automatically scale the covariates
# be very careful about the scale if you want to replicate the results
# since ridge is not invariant to scale transformation

xsd = apply(X, 2, sd)*sqrt((n-1) / n) # the (n-1)/n is due to the unbiased estimation of variance
x = scale(X)*sqrt(n / (n-1))
betahat = solve(t(x) %*% x + diag(c(5, 5))) %*% t(x) %*% (y - mean(y))
# recover the original beta 
betahat / xsd
# solve for intercept 
mean(y - X %*% (betahat / xsd))


# analysis on the prostate cancer example

library(ElemStatLearn)
library(glmnet)
library(MASS)
library(lars)
# the prostate cancer data is trying to predict lpsa, with the first 8 covariates
prostate 

# this interesting code is from online source that searches for the degrees of freedom of a given lambda
# you need to supply the eigen values
# I modified the later part of it

dof2lambda <- function(d, dof) {
    obj <- function(lam, dof) (dof - sum(d ^ 2 / (d ^ 2 + lam))) ^ 2
    sapply(dof, function(x) optimize(obj, c(0, 1e4), x)$minimum)
}

lambda2dof <- function(d, lam) {
    obj <- function(dof, lam) (dof - sum(d ^ 2 / (d ^ 2 + lam))) ^ 2
    sapply(lam, function(x) optimize(obj, c(0, length(d)), x)$minimum)
}

dat   <- prostate
train <- subset(dat,  train, select = -train)
test  <- subset(dat, !train, select = -train)


# WARNING!!!!!!!
# All the analysis below will use this standardized X and y! this is not the original scale!!!
# You cannot use the fitted coefficients for prediction!!!

train.x <- as.matrix(scale(subset(train, select = -lpsa)))
train.y <- as.matrix(scale(train$lpsa))

d   <- svd(train.x)$d
dof <- seq(0, 8, 0.01)
lam <- dof2lambda(d, dof)

# you can use either the lm.ridge or glmnet to fit the model 
# the two provides identical result

ridge1 <- lm.ridge(train.y ~ train.x, lambda = lam)
matplot(dof, t(ridge1$coef),  type = 'l', xlim = c(0, 9), lwd = 3, 
		xlab = "Degrees of Freedom", ylab = "coefficients", cex.lab = 1.5)
		
text(8.7, ridge2$beta[, ncol(ridge2$beta)], colnames(train.x), cex = 1.5)
abline(h = 0, lty = 3)

# the default selection criteria generalized cross validation error
select(ridge1)
# the best lambda based on GCV
lam[which.min(ridge1$GCV)]
# the degrees of freedom for best GCV
dof[which.min(ridge1$GCV)]


# keep in mind that for glmnet, the lambda is scaled with the number of observations 

ridge2 <- glmnet(train.x, train.y, alpha = 0, lambda = lam / nrow(train.x))
matplot(lambda2dof(d, ridge2$lambda * nrow(train.x)), t(ridge2$beta), type = 'l', xlim = c(0, 9), lwd = 3, 
		xlab = "Degrees of Freedom", ylab = "coefficients", cex.lab = 1.5)
text(8.7, ridge2$beta[, ncol(ridge2$beta)], colnames(train.x), cex = 1.5)
abline(h = 0, lty = 3)
title("Prostate Cancer Data: Ridge Regression", cex.main = 2)




# lasso fitting
lasso.fit = glmnet(train.x, train.y)

# use lambda as x-axes, see how the coefficients changes
plot(lasso.fit, xvar = "lambda")

# plot use L1 norm as x-axes
plot(lasso.fit)

# lars
lars.fit = lars(train.x, train.y, type = "lar")

# a piecewise linear solution path
plot(lars.fit, xvar = "norm", breaks = FALSE)

# stagewise regression
stagewise.fit = lars(train.x, train.y, type = "forward.stagewise")
plot(stagewise.fit, xvar = "norm", breaks = FALSE)

# forward stepwise regression
stepwise.fit = lars(train.x, train.y, type = "stepwise")
plot(stepwise.fit, xvar = "norm", breaks = FALSE)




