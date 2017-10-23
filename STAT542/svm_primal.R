# Question 1
## 1.1

### Generate dataset
set.seed(1)
n <- 40; p <- 2
xpos <- matrix(rnorm(n*p), n, p)
xneg <- matrix(rnorm(n*p, 4, 1), n, p)
x <- rbind(xpos, xneg)
y <- matrix(c(rep(1, n), rep(-1, n)))

library(ggplot2)
plot11 <- ggplot() +
  geom_point(mapping = aes(x = x[,1],
                           y = x[,2],
                           color = as.factor(y))) +
  xlab("x1") + ylab("x2") + labs("y")
plot11

### Performance from e1071
library(e1071)
model_svm <- svm(as.factor(y) ~ x, kernel = "linear", scale=FALSE)
predict(model_svm, newdata=x)
w <- t(model_svm$coefs) %*% model_svm$SV
b <- -model_svm$rho
plot11 + 
  geom_abline(intercept = -b/w[2],
              slope = -w[1]/w[2])


### Performance from primal
library(quadprog)
# https://stats.stackexchange.com/questions/179900/optimizing-a-support-vector-machine-with-quadratic-programming/180093
Dmat <- matrix(0, p+1, p+1); diag(Dmat) <- 10^-5 +1
Dmat[nrow(Dmat), nrow(Dmat)] <- 10^-5
dvec <- array(0, p+1)
Amat <- cbind(x, rep(1, nrow(y))) * as.vector(y)
bvec <- rep(1, nrow(y))
model_primal <- solve.QP(Dmat, dvec, t(Amat), bvec=bvec)

w <- model_primal$solution[1:2]
b <- model_primal$solution[3]
plot11 + 
  geom_abline(intercept = -b/w[2],
              slope = -w[1]/w[2])

## 1.2

