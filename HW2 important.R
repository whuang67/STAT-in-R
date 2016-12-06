set.seed(542)
vowelTrain <- read.table("~/vowel.train.data")
vowelTest <- read.table("~/vowel.test.data")

class <- array(data = 0, dim=c(48, 10, 11))
mean <- matrix(data = 0, nrow = 11, ncol= 10)
X <- array(data = 0, dim=c(48,10,11))
qx <- array(data = 0, dim=c(48,10,11))
scaling <- array(data = 0, dim=c(10,10,11))
logDet <- array(data = 0, dim = 11)
prior <- array(data=0, dim =11)


for (i in 1:11){
  class[,, i] = as.matrix(vowelTrain[vowelTrain$y== i ,-1])
  mean[i,] <- colMeans(class[,,i])
  X[,,i] <- scale(class[,,i], center=mean[i,], scale=FALSE)/sqrt(48-1)
  qx[,,i] <- qr(X[,,i])$qr
  scaling[,,i] <- backsolve(qx[1L:10,,i], diag(10)) # we get the R inverse?
  logDet[i] <- 2*sum(log(abs(diag(qx[,,i]))))
  prior[i] <- dim(class)[1]/dim(vowelTrain)[1]
}
solve(as.matrix(scaling[,,1]))

dist <- array(data=0, dim=c(462, 11))
dist <- data.frame(dist)
for (i in 1:11){
  dev <- as.matrix(vowelTest[,-1] - matrix(mean[i,], nrow = 462, ncol=10, byrow=TRUE)) %*% as.matrix(scaling[,,i])
  dist[,i] <- (-0.5*rowSums(dev^2) -0.5 * logDet[i] + log(prior[i]))
  names(dist)[i] <- i
}

pred <- rep(0,462)
for (i in 1:462){
  pred[i] <- which.max(dist[i,])
}

table(vowelTest[,1], pred)
mean(vowelTest[,1] == pred)

