library(MASS)
library(expm)
library(dr)
N = 500; P = 10
x = matrix(rnorm(N*P), N, P)
b = matrix(c(1, 1, rep(0, P-2)))
y = 0.125 * (x%*%b)^3 + 0.5*rnorm(N)
# X <- mvrnorm(N, mu = 3*runif(P)-1, Sigma = V)
# y <- X %*% c(1:5/5, rep(0, P-5)) + (X[, 5]+X[, 6])^3 + rnorm(N)

fit_sir = dr(y ~ ., data = data.frame(x, y), method = "sir")
fit_sir

sigmaX <- cov(x); X_EX <- scale(x, center=TRUE, scale=FALSE)
Z <- X_EX %*% sqrtm(solve(sigmaX))

idx <- order(y)
new_y <- y[idx]; new_Z <- Z[idx,]

idx_ = matrix(1:length(new_y), ncol=4)
find_M <- function(i) apply(new_Z[idx_[, 1], ], 2, mean)
c <- lapply(c(1,2,3,4), find_M)
M <- (c[[1]]%*%t(c[[1]]) + c[[2]]%*%t(c[[2]]) + c[[3]]%*%t(c[[3]]) + c[[4]]%*%t(c[[4]]))/4
alpha = eigen(M)$vectors
sqrtm(solve(sigmaX)) %*% alpha[,1]
sqrtm(solve(sigmaX)) %*% alpha[,2]
sqrtm(solve(sigmaX)) %*% alpha[,3]
sqrtm(solve(sigmaX)) %*% alpha[,4]


# sort(y)
# solve(sqrtm(sigmaX))
# apply(X_EX, 2, mean)

