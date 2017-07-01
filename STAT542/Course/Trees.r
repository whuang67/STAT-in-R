# STAT / CES 542, Fall 2016
# This is the R code for lecture note Trees

library(party)
library(rpart)
library(tree)

set.seed(1)
n = 500
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
y = rbinom(n, size = 1, prob = ifelse(x1^2 + x2^2 < 0.6, 0.9, 0.1))

par(mar=rep(2,4))
plot(x1, x2, col = ifelse(y == 1, "deepskyblue", "darkorange"), pch = 19, yaxt="n", xaxt = "n")
symbols(0, 0, circles = sqrt(0.6), add = TRUE, inches = FALSE, cex = 2, col = "gray")

rpart.fit = rpart(as.factor(y)~x1+x2, data = data.frame(x1, x2, y))
plot(rpart.fit)
text(rpart.fit)
plotcp(rpart.fit)
rpart.fit$cptable 
prune(rpart.fit, cp = 0.045)

tree.fit = tree(as.factor(y)~x1+x2, data = data.frame(x1, x2, y))
plot(tree.fit)
text(tree.fit)

par(mar=rep(2,4))
plot(x1, x2, col = ifelse(y == 1, "deepskyblue", "darkorange"), pch = 19, yaxt="n", xaxt = "n")

lines(x = c(-1, 1), y = c(-0.6444322, -0.6444322), lwd = 2)
lines(x = c(0.6941279, 0.6941279), y = c(-0.6444322, 1), lwd = 2)
lines(x = c(-1, 0.6941279), y = c(0.7484327, 0.7484327), lwd = 2)
lines(x = c(-0.6903174, -0.6903174), y = c(-0.6444322, 0.7484327), lwd = 2)

# the model will go further, but they seem to be over-fitting

lines(x = c(-0.7675897, -0.7675897), y = c(-0.6444322, 0.7484327), lwd = 2, lty = 2)
lines(x = c(-0.6903174, 0.6941279), y = c(0.3800769, 0.3800769), lwd = 2, lty = 2)


# compare Gini and Shannon entropy 

gini <- function(y)
{
	p = table(y)/length(y)
	sum(p*(1-p))
}

shannon <- function(y)
{
	p = table(y)/length(y)
	-sum(p*log(p))
}

error <- function(y)
{
	p = table(y)/length(y)
	1 - max(p)
}

score <- function(TL, TR, measure)
{
	nl = length(TL)
	nr = length(TR)
	n = nl + nr
	f <- get(measure)
	f(c(TL, TR)) - nl/n*f(TL) - nr/n*f(TR)
}

TL = rep(1, 3)
TR = c(rep(1, 4), rep(0, 3))

score(TL, TR, "gini")
score(TL, TR, "shannon")
score(TL, TR, "error")

x = seq(0, 1, 0.01)
g = 2*x*(1-x)
s = -x*log(x) - (1-x)*log(1-x)
e = 1-pmax(x, 1-x)

par(mar=c(4.2,4.2,2,2))
plot(x, s, type = "l", lty = 1, col = 3, lwd = 2, ylim = c(0, 1), ylab = "Impurity", xlab = "p", cex.lab = 1.5)
lines(x, g, lty = 1, col = 2, lwd = 2)
lines(x, e, lty = 1, col = 4, lwd = 2)

legend("topleft", c("Entropy", "Gini", "Error"), col = c(3,2,4), lty =1, cex = 1.2)





# bagging from ipred package

library(ipred)
library(rpart)
library(randomForest)

set.seed(1)
n = 400
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
y = rbinom(n, size = 1, prob = ifelse(x1^2 + x2^2 < 0.6, 0.8, 0.2))

xgrid = expand.grid(x1 = seq(-1, 1, 0.01), x2 = seq(-1, 1, 0.01))

# CART
rpart.fit = rpart(as.factor(y)~x1+x2, data = data.frame(x1, x2, y))
pred = matrix(predict(rpart.fit, xgrid, type = "class") == 1, 201, 201)
par(mar=rep(2,4))
contour(seq(-1, 1, 0.01), seq(-1, 1, 0.01), pred, levels=0.5, labels="",axes=FALSE)
points(x1, x2, col = ifelse(y == 1, "deepskyblue", "darkorange"), pch = 19, yaxt="n", xaxt = "n")
points(xgrid, pch=".", cex=1.2, col=ifelse(pred, "deepskyblue", "darkorange"))
box()

#Bagging
bag.fit = bagging(as.factor(y)~x1+x2, data = data.frame(x1, x2, y), nbagg = 200, ns = 400)
pred = matrix(predict(prune(bag.fit), xgrid) == 1, 201, 201)
par(mar=rep(2,4))
contour(seq(-1, 1, 0.01), seq(-1, 1, 0.01), pred, levels=0.5, labels="",axes=FALSE)
points(x1, x2, col = ifelse(y == 1, "deepskyblue", "darkorange"), pch = 19, yaxt="n", xaxt = "n")
points(xgrid, pch=".", cex=1.2, col=ifelse(pred, "deepskyblue", "darkorange"))
box()

# random forests
rf.fit = randomForest(cbind(x1, x2), as.factor(y), ntree = 1000, mtry = 1, nodesize = 25)
pred = matrix(predict(rf.fit, xgrid) == 1, 201, 201)
par(mar=rep(2,4))
contour(seq(-1, 1, 0.01), seq(-1, 1, 0.01), pred, levels=0.5, labels="",axes=FALSE)
points(x1, x2, col = ifelse(y == 1, "deepskyblue", "darkorange"), pch = 19, yaxt="n", xaxt = "n")
points(xgrid, pch=".", cex=1.2, col=ifelse(pred, "deepskyblue", "darkorange"))
box()




# variable selection via importance 

n = 400
p = 50
x = matrix(runif(n*p, -1, 1), n, p)
colnames(x) = paste("x", 1:p, sep = '')
y = rbinom(n, size = 1, prob = ifelse(x[,1]^2 + x[,2]^2 < 0.6, 0.7, 0.3))

rf.fit = randomForest(x, as.factor(y), ntree = 1000, mtry = p/3, nodesize = 5, importance = TRUE)
par(mar=rep(2,4))
barplot(importance(rf.fit)[,3])






