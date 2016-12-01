# STAT / CES 542, Fall 2016
# This is the R code for lecture note Boosting

library(gbm)

# the current gbm package seems to have bug when cross validation is used with just one predictor
# I am not sure why, but just be aware of this issue, it may get fixed later on

y <- sample(0:1, 100, replace = TRUE)
x = rnorm(100)
gbm(y ~ ., data = data.frame(x, y)) # this works fine
gbm(y ~ ., data = data.frame(x, y), cv.folds = 3) # this produces an error


# lets do a simple example

p = 1 
x = seq(0, 1, 0.001)
py = (sin(4*pi*x) + 1)/2
plot(x, py, type = "l", lwd = 3, ylab = "P(Y=1 | X=x)")

y = rbinom(length(x), 1, py)
points(x, y, pch = 19, col = ifelse(y==1, "red", "blue"))

# fit AdaBoost with bootstrapping

gbm.fit = gbm(y~x, distribution="adaboost", n.trees=50, shrinkage=1, bag.fraction=0.8)

gbm.perf(gbm.fit)

par(mfrow=c(2,3))
size=c(1,2,3,4,16,30)

for(i in 1:6)
{
	par(mar=c(2,2,3,1))
	plot(x, py, type = "l", lwd = 3, ylab = "P(Y=1 | X=x)", col = "gray")
	points(x, y, pch = 19, col = ifelse(y==1, "red", "blue"))
	Fx = predict(gbm.fit, n.trees=size[i]) # this returns the fitted function, but not class
 	lines(x, 1/(1+exp(-2*Fx)), lwd = 3)
	title(paste("# of Iterations = ", size[i]))
}

# changing to a smaller shrinkage factor

gbm.fit = gbm(y~x, distribution="adaboost", n.trees=1000, shrinkage=0.1, bag.fraction=0.8)

gbm.perf(gbm.fit)

par(mfrow=c(2,3))
size=c(1,2,3,50,100,1000) # 1000 trees is clearly an over-fitting

for(i in 1:6)
{
	plot(x, py, type = "l", lwd = 3, ylab = "P(Y=1 | X=x)", col = "gray")
	points(x, y, pch = 19, col = ifelse(y==1, "red", "blue"))
	Fx = predict(gbm.fit, n.trees=size[i]) # this returns the fitted function, but not class
 	lines(x, 1/(1+exp(-2*Fx)), lwd = 3)
	title(paste("# of Iterations = ", size[i]))
}


# with cross-validation (I have to create other covariates here otherwise it produces error...)

gbm.fit = gbm(y~., data = data.frame(x, x2 = matrix(runif(length(x))), y), 
		distribution="adaboost", n.trees=100, shrinkage=1, bag.fraction=0.8, cv.folds=10)

usetree = gbm.perf(gbm.fit, method="cv")

plot(x, py, type = "l", lwd = 3, ylab = "P(Y=1 | X=x)", col = "gray")
points(x, y, pch = 19, col = ifelse(y==1, "red", "blue"))
Fx = predict(gbm.fit, n.trees=usetree) # this returns the fitted function, but not class
lines(x, 1/(1+exp(-2*Fx)), lwd = 3)
title(paste("# of Iterations = ", usetree))

# you can peek each tree
pretty.gbm.tree(gbm.fit, i.tree = 1)


# our previous circle example 
set.seed(1)
n = 500
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
y = rbinom(n, size = 1, prob = ifelse(x1^2 + x2^2 < 0.6, 0.8, 0.2))
xgrid = expand.grid(x1 = seq(-1, 1, 0.01), x2 = seq(-1, 1, 0.01))

# one variable in each tree

gbm.fit = gbm(y~., data = data.frame(x1, x2, y), 
		distribution="adaboost", n.trees=1000, shrinkage=0.01, bag.fraction=0.8, interaction.depth = 1, cv.folds=10)

usetree = gbm.perf(gbm.fit, method="cv")
Fx = predict(gbm.fit, xgrid, n.trees=usetree)

pred = matrix(1/(1+exp(-2*Fx)) > 0.5, 201, 201)
par(mar=rep(2,4))
contour(seq(-1, 1, 0.01), seq(-1, 1, 0.01), pred, levels=0.5, labels="",axes=FALSE)
points(x1, x2, col = ifelse(y == 1, "deepskyblue", "darkorange"), pch = 19, yaxt="n", xaxt = "n")
points(xgrid, pch=".", cex=1.2, col=ifelse(pred, "deepskyblue", "darkorange"))
box()
pretty.gbm.tree(gbm.fit, i.tree = 1)

# allow interactions in each tree

gbm.fit = gbm(y~., data = data.frame(x1, x2, y), 
		distribution="adaboost", n.trees=1000, shrinkage=0.01, bag.fraction=0.8, interaction.depth = 2, cv.folds=10)

usetree = gbm.perf(gbm.fit, method="cv") 
Fx = predict(gbm.fit, xgrid, n.trees=usetree)

pred = matrix(1/(1+exp(-2*Fx)) > 0.5, 201, 201)
par(mar=rep(2,4))
contour(seq(-1, 1, 0.01), seq(-1, 1, 0.01), pred, levels=0.5, labels="",axes=FALSE)
points(x1, x2, col = ifelse(y == 1, "deepskyblue", "darkorange"), pch = 19, yaxt="n", xaxt = "n")
points(xgrid, pch=".", cex=1.2, col=ifelse(pred, "deepskyblue", "darkorange"))
box()

pretty.gbm.tree(gbm.fit, i.tree = 1)