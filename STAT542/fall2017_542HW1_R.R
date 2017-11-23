### Question 1 ######################################
library(MASS)
set.seed(1)
P = 4
N = 200
rho = 0.5
V <- rho^abs(outer(1:P, 1:P, "-"))
X = as.matrix(mvrnorm(N, mu=rep(0,P), Sigma=V))
beta = as.matrix(c(1, 1, 0.5, 0.5))
Y = X %*% beta + rnorm(N)

### a ###
sample_var_cov <- function(X){
  n = nrow(X)
  X_ = scale(X, center=TRUE, scale=FALSE)
  return(t(X_) %*% X_ /n)
}
sample_var_cov(X)

### b ###
mydist <- function(x1, x2) sqrt(sum((x1-x2)^2))

KNN_point <- function(testing_point, training_set, training_label, dist_function=mydist, k=5, ...){
  distance <- apply(X, MARGIN=1, FUN=dist_function, x1=testing_point, ...)
  index <- sort.int(distance, index.return=TRUE)$ix[seq(1, k)]
  return(mean(training_label[index]))
}

KNN_point(c(0.5, 0.5, 0.5, 0.5), X, Y)

### c ###
# mydist2 <- function(x1, x2, s)


### Question 2 ######################################
### b ###
y_real <- array(NA, dim=c(200, 20)); y_pred <- array(NA, dim=c(200, 20))
corr <- array(NA, 20)
set.seed(1)
for(i in 1:20){
  y_real[, i] = X %*% beta + rnorm(N)
  y_pred[, i] <- apply(X, MARGIN=1, FUN=KNN_point, training_set=X, training_label=y_real[, i])
  # corr[i] <- cor(y_real, y_pred)
}
sum(diag(cov(t(y_real), t(y_pred))))

### Question 3 ######################################
library(class)
library(ElemStatLearn)
dat <- SAheart

for(i in seq(1, 31, 2)){
  model <- knn.cv()
}

# then the estimated beta is 
hbeta = solve(t(X) %*% X) %*% t(X) %*% Y
hbeta