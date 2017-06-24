# STAT / CES 542, Fall 2016
# This is the R code for lecture note SIR

library(scatterplot3d)
library(dr)
library(MASS)

set.seed(10)

library(rgl)
library(akima)


n = 5000; p = 10
x = matrix(rnorm(n*p), n, p)
b = matrix(c(1, 1, rep(0, p-2)))
y = x[,1] + 0.5*rnorm(n)

fit = dr(y~., data = data.frame(x, y), method = "save")

# generate some data with one direction

n = 5000; p = 10
x = matrix(rnorm(n*p), n, p)
b = matrix(c(1, 1, rep(0, p-2)))
y = 0.125*(x %*% b)^3 + 0.5*rnorm(n)

fit.sir = dr(y~., data = data.frame(x, y), method = "sir")

# mfrow3d(2, 2)
# visualize the data

plot3d(x[,1], x[, 2], y, col="red", size=5)
s = interp(x[,1], x[, 2], 0.125*(x %*% b)^3)
surface3d(s$x, s$y, s$z, col = 'gray', alpha = 0.4, add = T)

# e = eigen(cov(x))
# sigmaneghalf = e$vector %*% diag(1/sqrt(e$value)) %*% t(e$vector)
# z = x %*% sigmaneghalf

# calculate the inverse regression curve (I did not diagonalize cov(x) because the variables are independent with mean 0)
mydata = data.frame("x" = x, "y"= y)
mydata = mydata[order(mydata$y), ]

H = 20
id = split(1:n, cut(seq_along(1:n), H, labels = FALSE))
means = sapply(1:H, function(h, mydata, id, p) colMeans(mydata[id[[h]], ]), mydata, id)

# the inverse curve moves in the direction of X_1 + X_2
lines3d(x = means[1, ], y = means[2, ], z = rep(5, H), col = "blue", lwd = 3, add = T)
pch3d(x = means[1, ], y = means[2, ], z = rep(5, H), col = "blue", pch = 1, cex = 0.4, add = T)

# the other variables will not change much as we move the slices 
plot(means[3, ], ylim = c(-1.96, 1.96), xlab = "slice", ylab = "E(X3)")

# plot the inverse regression curve on the joint direction, and also two other arbitrary directions that is orthogonal to it
# again, I did not diagonalize cov(x) because the variables are independent

plot3d(x = sqrt(1/2)*t(b) %*% means[1:p, ], y = means[3,], z = means[4,], 
		xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2), 
		xlab = "Joint direction of X1 + X2", ylab = "X3", zlab = "X4", col = "blue", size = 5)
lines3d(x =sqrt(1/2)*t(b) %*% means[1:p, ], y = means[3,], z = means[4,], col = "blue", lwd = 3, add = T)


# generate some data with two directions, and also with correlations

n = 5000
p = 10
V = 0.5^abs(outer(1:p, 1:p, "-"))  

x = mvrnorm(n, rep(0, p), V)

b1 = matrix(c(sqrt(0.5), 0, sqrt(0.5), 0, rep(0, p-4)))
b2 = matrix(c(0, sqrt(0.5), 0, -sqrt(0.5), rep(0, p-4)))

dir1 = x %*% b1
dir2 = x %*% b2

link = dir1 + 4*atan(2*dir2)
y = link + 0.5*rnorm(n)

# visualize the data in the true directions
# of course this is only possible if you know the truth

plot3d(dir1, dir2, y, col="red", size=3)
s = interp(dir1, dir2, link)
surface3d(s$x, s$y, s$z, col = 'gray', alpha = 0.4, add = TRUE)

# if we fit a linear model 
ylin = lm(y ~ x)$fitted.values
s2 = interp(dir1, dir2, ylin)
surface3d(s2$x, s2$y, s2$z, col = 'green', alpha = 0.4, add = TRUE)
mean((ylin - link)^2) # pretty bad 

# calculate the inverse regression curve
mydata = data.frame("x" = x, "y"= y)
mydata = mydata[order(mydata$y), ]
H = 30
id = split(1:n, cut(seq_along(1:n), H, labels = FALSE))
means = sapply(1:H, function(h, mydata, id, p) colMeans(mydata[id[[h]], ]), mydata, id)
means = solve(V) %*% means[1:p, ] # a sloppy way

# the inverse curve moves in the space defined by two directions
# but will not change much over some arbitrary third direction that is orthogonal to b1 and b2
b3 = matrix(c(0, 0, 0, 0, rep(sqrt(1/6), 6)))
plot3d(x = t(b1) %*% means, y = t(b2) %*% means, z = t(b3) %*% means, 
		xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2), 
		xlab = "dir 1", ylab = "dir 2", zlab = "any other direction", col = "blue", size = 5)

# fit a sliced inverse regression
fit.sir = dr(y~., data = data.frame(x, y), method = "sir")

# construct the estimated directions 
d1 = x %*% fit.sir$evectors[,1,drop = FALSE]
d2 = x %*% fit.sir$evectors[,2,drop = FALSE]

# fit a two dimensional kernel regression using these two derived directions
refit = loess(y ~ d1 + d2, span = 0.03, degree = 2) # you cannot do this for the original data with 10 dimensions, thats just bad
mean((predict(refit) - link)^2) # this fitting is very good

# visualize the new construction
plot3d(d1, d2, y, col="red", size=3)
s3 = interp(d1, d2, predict(refit))
surface3d(s3$x, s3$y, s3$z, col = 'green', alpha = 0.4, add = TRUE)

# plot the fitted value on the original true directions 

s4 = interp(dir1, dir2, predict(refit))
plot3d(dir1, dir2, y, col="red", size=3)
surface3d(s$x, s$y, s$z, col = 'gray', alpha = 0.4, add = TRUE)
surface3d(s$x, s$y, s4$z, col = 'green', alpha = 0.4, add = TRUE)







# lets do a new function 

n = 5000
p = 10

x = matrix(rnorm(n*p), n, p)

b1 = matrix(c(1, 0, 1, rep(0, p-3)))
b2 = matrix(c(0, 1, 0, 1, 1, rep(0, p-5)))

dir1 = x %*% b1
dir2 = x %*% b2

link = 4*sin(dir1) + 0.5*(dir2)^2
y = link + 0.5*rnorm(n)

# visualize the data 

plot3d(dir1, dir2, y, col="red", size=3)
s = interp(dir1, dir2, link)
surface3d(s$x, s$y, s$z, col = 'gray', alpha = 0.4, add = TRUE)

# save will recover the direction better in this case

fit.save = dr(y~., data = data.frame(x, y), method = "save")

# sir cannot detect direction 2
fit.sir = dr(y~., data = data.frame(x, y), method = "sir")

# construct the estimated directions 

d1 = x %*% fit.save$evectors[,1,drop = FALSE]
d2 = x %*% fit.save$evectors[,2,drop = FALSE]

plot3d(d1, d2, y, col="red", size=3)
refit = loess(y ~ d1 + d2, span = 0.03, degree = 2) # you cannot do this for the original data with 10 dimensions, thats just bad
mean((predict(refit) - link)^2)

# see the fitted value on the true directions 
plot3d(dir1, dir2, y, col="red", size=3)
s2 = interp(dir1, dir2, predict(refit))
surface3d(s2$x, s2$y, s2$z, col = 'green', alpha = 0.4, add = TRUE)



