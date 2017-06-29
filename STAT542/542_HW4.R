### Question 1

set.seed(1)

require(MASS)
require(dr)
require(CCA)

n = 500
p = 10
V = 0.5^abs(outer(1:p, 1:p, "-"))  

x = mvrnorm(n, runif(p), V)

b1 = matrix(c(sqrt(0.5), 0, sqrt(0.5), 0, rep(0, p-4)))
b2 = matrix(c(0, sqrt(0.5), 0, -sqrt(0.5), rep(0, p-4)))

dir1 = x %*% b1
dir2 = x %*% b2

link = dir1 + 4*atan(2*dir2)
y = link + 0.5*rnorm(n)

# use the dr package, and perform SIR using 10 slices 

fit.sir = dr(y~., data = data.frame(x, y), method = "sir", nslices = 10)
# fit.sir$evectors[, 1:2]
# cc(fit.sir$evectors[, 1:2], cbind(b1, b2))$cor



# Step 1
CovMatrix <- stats::cov(x)
Z <- base::scale(x, center = colMeans(x), scale = FALSE) %*% base::solve(expm::sqrtm(CovMatrix))

# Step 2
ZY <- base::cbind(Z,y)
SortOrder <- base::order(ZY[,11], decreasing = FALSE, na.last = TRUE)
Sorted.ZY <- ZY[SortOrder,]
z <- base::array(0, dim = c(n/10, 11, 10))
for (i in 1:10){
  z[, , i] <- Sorted.ZY[(n/10*(i-1)+1): (n/10*i), ]
}

# Step 3
zhbarY <- base::array(0, dim = c(10, 11))
for (i in 1:10){
  zhbarY[i, ] <- colMeans(z[, , i])
}
zhbar <- t(zhbarY[, -11])
zbarY <- as.vector(colMeans(Sorted.ZY))
zbar <- zbarY[-11]

# Step 4
m.new <- base::array(0, dim = c(10, 10))
m.previous <- base::array(0, dim = c(10, 10))
for(i in 1:10){
  m.new = (as.vector(zhbar[, i]) -as.vector( zbar)) %*% t(as.vector(zhbar[, i]) - as.vector(zbar))* (1/10)
  m.new = m.new + m.previous
  m.previous = m.new
}
M <- m.new

# Step 5
PCA <- stats::prcomp(M)
beta1.beta2 <- (solve(expm::sqrtm(CovMatrix)) %*% PCA$rotation)[,1:2]
# cc(beta1.beta2, cbind(b1,b2))$cor


# Visualize the result
library(knitr)
BETA1.BETA2 <- cbind(data.frame(beta1.beta2), data.frame(fit.sir$evectors[, 1:2]))
names(BETA1.BETA2) <- c("Dir1.own", "Dir2.own", "Dir1.dr", "Dir2.dr")
knitr::kable(BETA1.BETA2,
             align = "c",
             caption = "Results of both Own and Package Codes")

### Change n to 50000

n = 50000
p = 10
V = 0.5^abs(outer(1:p, 1:p, "-"))  

x = mvrnorm(n, runif(p), V)

b1 = matrix(c(sqrt(0.5), 0, sqrt(0.5), 0, rep(0, p-4)))
b2 = matrix(c(0, sqrt(0.5), 0, -sqrt(0.5), rep(0, p-4)))

dir1 = x %*% b1
dir2 = x %*% b2

link = dir1 + 4*atan(2*dir2)
y = link + 0.5*rnorm(n)

### Repeat the previous part, then visualize the plot

N50000 <- data.frame(t(cc(beta1.beta2, cbind(b1,b2))$cor))
names(N50000) <- c("cc1", "cc2")
row.names(N50000) <- "Canonical Correlations"
knitr::kable(N50000,
             align = "c",
             caption = "Results of CCA (n=50000)")



### Change it back to 500
n = 500
p = 10
V = 0.5^abs(outer(1:p, 1:p, "-"))  

x = mvrnorm(n, runif(p), V)

b1 = matrix(c(sqrt(0.5), 0, sqrt(0.5), 0, rep(0, p-4)))
b2 = matrix(c(0, sqrt(0.5), 0, -sqrt(0.5), rep(0, p-4)))

dir1 = x %*% b1
dir2 = x %*% b2

link = dir1 + 4*atan(2*dir2)
y = link + 0.5*rnorm(n)


### Repeat the previous part, then visualize the plot

N500 <- data.frame(t(cc(fit.sir$evectors[, 1:2], beta1.beta2)$cor))
names(N500) <- c("cc1", "cc2")
row.names(N500) <- "Canonical Correlations"
knitr::kable(N500,
             align = "c",
             caption = "Results of CCA (n=500)")



## Question 2


set.seed(1)
library(ElemStatLearn)
Spam <- ElemStatLearn::spam

library(gbm)
library(caret)

caretGrid <- expand.grid(interaction.depth=1,
                         n.trees = c(300, 350, 400),
                         shrinkage=c(0.01, 0.1, 0.99),
                         n.minobsinnode=10)
trainControl <- trainControl(method="cv", number=10)

boosting.Logistic <- train(
  spam ~ .,
  data = Spam,
  method = "gbm",
  distribution = "adaboost",
  trControl = trainControl,
  tuneGrid = caretGrid,
  verbose = FALSE
)

boosting.Exponential <- train(
  spam ~ .,
  data = Spam,
  method = "gbm",
  distribution = "bernoulli",
  trControl = trainControl,
  tuneGrid = caretGrid,
  verbose = FALSE
)


Exponential.result <- data.frame(boosting.Exponential$results)[,c(1,4,5)]
names(Exponential.result) <- c("shrinkage.exp", "ntrees.exp", "Accuracy.exp")
row.names(Exponential.result) <- NULL
Logistic.result <- data.frame(boosting.Logistic$results)[,c(1,4,5)]
names(Logistic.result) <- c("shrinkage.log", "ntrees.log", "Accuracy.log")
row.names(Logistic.result) <- NULL
knitr::kable(cbind(Exponential.result, Logistic.result),
             align = "c",
             caption = "Results of Model Fitting")


BestTune <- data.frame(
  rbind(
    boosting.Exponential$bestTune[c(1,3)],
    boosting.Logistic$bestTune[c(1,3)]
    ),
  c("Exponential Loss", "Logistic Likelihood Loss"),
  c(max(boosting.Exponential$results$Accuracy),
    max(boosting.Logistic$results$Accuracy))
)

row.names(BestTune) <- NULL
names(BestTune)[c(3,4)] <- c("Loss", "Accuracy")
knitr::kable(BestTune,
             align = "c",
             caption = "Best Tuning Parameters of Both Methods")

