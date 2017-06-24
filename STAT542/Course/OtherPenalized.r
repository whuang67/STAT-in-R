# STAT / CES 542, Fall 2016
# This is the R code for lecture note Penalized2
# This file includes model fitting of many different penalized methods


library(glmnet)
library(MASS)


set.seed(1)
n = 100
p = 300

# correlation matrix 
Sigma = matrix(0.5, p, p)
diag(Sigma) = 1

# generate data
X = mvrnorm(n, rep(0, p), Sigma)
true_beta = matrix(c(0.5, 0, 0.5, 0, 0.5, rep(0, p-5)))
y = 1 + X %*% true_beta + rnorm(n)

# initial lasso fit
lasso.fit = cv.glmnet(X, y, nfolds = 10)

# check parameter estimates and how many of them are correct?
lasso.beta = coef(lasso.fit, s = "lambda.1se")
sum(lasso.beta!=0) # number of nonzero should be 10
sum(lasso.beta[-1]*true_beta !=0) # correct selection

sum((as.matrix(lasso.beta[-1]) - true_beta)^2)


# adaptive lasso matrix
X2 = sweep(X, 2, as.matrix(lasso.beta[-1]), "*")

Alasso.fit = cv.glmnet(X2, y, nfolds = 10, standardize = FALSE)
Alasso.beta = coef(Alasso.fit, s = "lambda.min")
sum(Alasso.beta!=0) # number of nonzero should be 10
sum(Alasso.beta[-1]*true_beta !=0) # correct selection

# the parameter estimates are the product of the two
mybeta = Alasso.beta*lasso.beta
# recover the intercept
mybeta[1] = mean(y - X %*% mybeta[-1])

sum((as.matrix(mybeta[-1]) - true_beta)^2)


# the SCAD and MCP penalties can be carried out using "ncvreg"
library(ncvreg)

# MCP
mcp.fit = cv.ncvreg(X, y, penalty="MCP", gamma = 2)
plot(mcp.fit)
mcp.beta = mcp.fit$fit$beta[, mcp.fit$min]
sum(mcp.beta!=0)
sum((as.matrix(mcp.beta[-1]) - true_beta)^2)

plot(ncvreg(X, y, penalty="MCP", gamma = 2), lwd = 3)

# SCAD

scad.fit = cv.ncvreg(X, y, penalty="SCAD", gamma = 3.7)
plot(scad.fit)
scad.beta = scad.fit$fit$beta[, scad.fit$min]
sum(scad.beta!=0)
sum((as.matrix(scad.beta[-1]) - true_beta)^2)

plot(ncvreg(X, y, penalty="SCAD", gamma = 3.7), lwd = 3)












