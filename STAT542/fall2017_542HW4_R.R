###### Question 1 #######################################################
#########################################################################
#########################################################################
###### a ################################################################
NadarayaWatson <- function(x, y, test=x, bandwidth=1){
  Gaussian_Kernal <- function(x1, x2) 1/sqrt(2*pi) * exp(-sum(((x1-x2)/bandwidth)^2))
  
  EachStep <- function(test){
    K = apply(x, MARGIN=1, FUN=Gaussian_Kernal, test)
    return(sum(K*y) / sum(K))
  }
  
  output <- apply(x, MARGIN=1, FUN=EachStep)
  return(output) 
}

get_RMSE <- function(y_real, y_pred) (mean((y_real-y_pred)^2))^.5

P = 20; N = 200
X = as.matrix(mvrnorm(N, mu=rep(0,P), Sigma=diag(P)))
y = apply(X, 1, sum) + rnorm(N)

X_test = as.matrix(mvrnorm(40, mu=rep(0,P), Sigma=diag(P)))
y_test = apply(X_test, 1, sum) + rnorm(40)

get_RMSE(y, NadarayaWatson(X, y))
get_RMSE(y_test, NadarayaWatson(X, y, X_test))

###### b ################################################################
dat = read.csv("C:/users/whuang67/downloads/Video_Games_Sales_as_at_22_Dec_2016.csv")
y = log(dat$Global_Sales + 1)
X = subset(dat, select = c("Critic_Score", "Critic_Count", "User_Score"))
X$User_Score = as.numeric(as.character(X$User_Score))
dat1 = na.omit(data.frame(y, X))


###### Question 2 #######################################################
#########################################################################
#########################################################################
###### a ################################################################
library(MASS)
set.seed(1)
P = 20; N = 200
X = as.matrix(mvrnorm(N, mu=rep(0,P), Sigma=diag(P)))
beta = c(.5, .5, .5, .5)
Y_mean = 1 + X[, 1:4] %*% beta

library(randomForest)
mtry_ = c(2, 3, 4, 5); nodesize_ = c(1, 2, 3, 4)
Y_real = array(0, dim=c(N, 20)); Y_pred = array(0, dim=c(N, 20))
cov_matrix = array(0, c(4, 4))
for(i in 1:4){
  for(j in 1:4){
    for(k in 1:20){
      Y_real[, k] =  Y_mean + rnorm(N)
      dat = data.frame(y=Y_real[, k], X)
      model <- randomForest(y ~ ., data = dat,
                            ntree = 10,
                            mtry=mtry_[i],
                            nodesize=nodesize_[j])
      Y_pred[, k] = predict(model, newdata = dat, type="response")
    }
    cov_matrix[i, j] = sum(diag(cov(t(Y_real), t(Y_pred))))
  }
}
cov_matrix

###### b ################################################################
y_real = Y_mean + rnorm(N)
for(tree in c(100, 200, 500, 1000)){
  model <- randomForest(X, y_real, ntree = tree)
  y_pred = predict(model, X)
  print(sd(y_pred) * (N-1)/N)
}


###### Question 3 #######################################################
#########################################################################
#########################################################################
###### a ################################################################
stump <- function(x, y, w = rep(1, length(x))/length(x)){
  ordered_x <- sort(x); max_score = NA; cut = NA
  
  for(i in ordered_x[-length(ordered_x)]){
    x_L <- x[x<=i]; y_L <- y[x<=i]; w_L <- w[x<=i]
    x_R <- x[x>i];  y_R <- y[x>i];  w_R <- w[x>i]
    
    p_L <- sum(w_L*(y_L==1))/sum(w_L); p_R <- sum(w_R*(y_R==1))/sum(w_R)
    
    Gini_L <- p_L*(1-p_L); Gini_R <- p_R*(1-p_R)
    
    score <- - (sum(w_L)*Gini_L + sum(w_R)*Gini_R)/sum(w)
    
    if(score > max_score | is.na(max_score)){
      max_score = score
      cut = i
    }
    
  y_predict <- ifelse(x<=cut, 1, -1)
  }
  return(list(cut = cut,
              predict = y_predict))
}


### Test
set.seed(1)
x = c(rnorm(100, mean = 1, sd = 1), rnorm(100, mean = 3, sd = 1))
y = c(rep(1,100), rep(-1,100))
w = rep(1,length(x))/length(x)
result <- stump(x, y, w)

## install.packages("ggplot2")
library(ggplot2)
ggplot() + geom_point(mapping = aes(x = x, y = y)) + geom_vline(xintercept=result$cut, color="red")

###### b ################################################################
Adaboost <- function(x, y, step = 1000){
  w = rep(1, length(x))/length(x)
  alpha = rep(NA, step); predict = array(NA, dim=c(length(y), step))
  
  for(i in 1:step){

    ## Bootstrap #################################################
    idx <- unique(sample(1:length(x), length(x), replace=TRUE))
    x_ <- x[idx]; y_ <- y[idx]; w_ <- w[idx]
    ##############################################################
    result <- stump(x_, y_, w_)

    cutoff <- result$cut
    predict[, i] = ifelse(x <= cutoff, 1, -1)
    error_t <- sum((predict[, i] != y)*w)/sum(w)
    
    alpha[i] <- 1/2 * log((1-error_t)/error_t)
    w_ <- w * exp(- alpha[i] * y*predict[, i])
    w <- w_/sum(w_)
  }
  
  score = predict %*% alpha
  final_pred <- ifelse(score > 0, 1, -1)
  
  return(final_pred)
}


### Test
n = 300
set.seed(1)
x = runif(n)
y = (rbinom(n , 1 , (sin(4*pi*x)+1)/2)-0.5)*2
ggplot(mapping = aes(x = x, y = y)) + geom_point()

#### Single CART
result <- stump(x, y)
mean(result$predict == y)

#### Adaboost
result <- Adaboost(x, y)
mean(result == y)
