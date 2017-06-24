#####  Script for Data exploration  ##############
## Explore numeric features , find out principal components and correlation analysis, 
# check for feasible transformations w.r.to response.
## Explore categorical variables , check for chi-square independence, 
# anova model for vars with few levels, vars with many levels may require special attention.
# try merging several vars and see if it makes sense.
##  outlier and influence diagnostics .
## try different feature intercations such as x1+x2  ,x1/x2, x1*x2 w.r.to response.
## reserve 10% of data for validation, dont fit any model for this data.
## Try both bootstrap and CV for regularization.
##  Training dataset has 180 k rows approx...better use 'readr' package for loading data it is faster
## for exploration 'dplyr' package is faster with large data.
##  Apart from CARET we can use H20 package which has nice models that can do distributed modelling(faster)

set.seed(542)
## Data loading
library(readr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(caret)
train <- read.csv("C:/Users/Jay/Desktop/542 project/train.csv")
test <- read.csv("C:/Users/Jay/Desktop/542 project/test.csv")
dim(train)
dim(test)
#train$loss=NULL
#train$label='TR'
#test$label='TE'
#all=rbind(train,test)
#library(Matrix)
#all_sparse_mat=sparse.model.matrix(loss~.,data = all)


#summary(train)
#str(train[,1:131])

#train=train[1:6000,]

## Missing values plot
library(Amelia)
missmap(train, col=c("red", "blue"), legend=TRUE) # no missing data

## correlation plot between numeric vars
numeric_data<- train[,118:131]
dim(numeric_data)
library(corrplot)
M <- cor(numeric_data)
#col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
#                           "cyan", "#007FFF", "blue","#00007F")) 
corrplot.mixed(M, lower="circle",upper="number",col=c("green","blue"))

## PCA for numeric data
summary(numeric_data)
pr.comp=prcomp(numeric_data,center = TRUE,scale. = TRUE)
pr.var=pr.comp$sdev^2
pve=pr.var/sum(pr.var)
pve
summary(pr.comp)
pve_cum=pve
for (i in 1:length(pve)){
  pve_cum[i+1]=pve_cum[i]+pve_cum[i+1]
}
pve_cum=na.omit(pve_cum)
number_ticks <- function(n) {function(limits) pretty(limits, n)}
ggplot()+geom_line(aes(x=(1:length(pve)),y=pve),size=1,col="green")+
  geom_line(aes(x=(1:length(pve)),y=pve_cum),col="magenta",size=1.25,linetype=4)+
  xlab("Number of principal components")+ylab("Percentage variance explained")+
  ggtitle("PCA plot")+scale_x_continuous(breaks=number_ticks(15))
# PCA seems useful just keep only top 3-4 components instead of 14 vars

# boxplot for loss /resposne
summary(train$loss)
boxplot(train$loss)
# histogram for loss
ggplot(data=train, aes(train$loss)) + geom_histogram(col="#f04546")+
  xlab("Number of observations")+ylab("loss value")+ggtitle("Histogram for loss")
# making loss as normal
ggplot(data=train, aes(log(train$loss))) + geom_histogram(fill="#f04546")+
  xlab("Log Loss Value")+ylab("Number of observations")+ggtitle("Histogram for log(loss)")
#+coord_flip()

train$loss=log(train$loss)

# there are some outleirs to be taken care
#ggplot()+geom_boxplot(aes(train$loss))


##### printing levels in the cat vars if two vars has same levels display msg
library(MASS)
tr_vars=names(train)[2:117]
attach(train)
for (i in 1:length(tr_vars))
{
  cat("cat variable",i,"has",length(table(train[,i+1])),"levels" )
  cat("\n")
} 

### Chi square test for independence
library(MASS)
#train=train[1:1000,]
tr_vars=names(train)[,2:117]
attach(train)
varlist=rep(0,500)
for (i in 1:length(tr_vars))
{
  for (j in 1:length(tr_vars))
  {
    if (i!=j)
    {
      x=t(train[,tr_vars[1]])
      y=t(train[,tr_vars[2]])
      tbl = table(x,y) 
      tbl 
      test_res=chisq.test(tbl)
      if (test_res$p.value<0.05){
        varlist=tr_vars[j]
        cat(tr_vars[i],tr_vars[j]," are dependent")
        cat("\n")
      }
    }
  }
} 


############################################################
####     NOTE : running this may take hours ################
############################################################

# gower distance for clustering for training dataset
memory.limit(size=250000)
library(cluster)
library(Rtsne)
library(data.table)
#train_mat=as.data.table(train)
#train_mat$loss=train$loss

library(Matrix)
#all_sparse_mat=Sprase.model.matrix(loss~.,data = train_mat)
train$id=NULL
train$loss=NULL

gower_dist <- daisy(train,metric = "gower")

#summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

#train1[
#  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
#        arr.ind = TRUE)[1, ], ]

pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

#pam_results <- college_clean %>%
#  dplyr::select(-name) %>%
#  mutate(cluster = pam_fit$clustering) %>%
#  group_by(cluster) %>%
#  do(the_summary = summary(.))
#pam_results$the_summary



tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
library(dplyr)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
library(ggplot2)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))+ggtitle("Clustering based on gower distance")


train$cluster=pam_fit$clustering
#getwd()
#write.csv(train,'train_clust.csv',row.names = FALSE)
kmo=as.data.frame(pam_fit$clustering)
names(kmo)=c("cluster")
write.csv(kmo,'tr_clust.csv',row.names = FALSE)


# gower distance for clustering for testing dataset
memory.limit(size=250000)
test$id=NULL


gower_dist <- daisy(test,metric = "gower")

#summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

#train1[
#  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
#        arr.ind = TRUE)[1, ], ]

pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

#pam_results <- college_clean %>%
#  dplyr::select(-name) %>%
#  mutate(cluster = pam_fit$clustering) %>%
#  group_by(cluster) %>%
#  do(the_summary = summary(.))
#pam_results$the_summary



tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
library(dplyr)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
library(ggplot2)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))+ggtitle("Clustering based on gower distance")


test$cluster=pam_fit$clustering
#getwd()
#write.csv(train,'train_clust.csv',row.names = FALSE)
kmo=as.data.frame(pam_fit$clustering)
names(kmo)=c("cluster")
write.csv(kmo,'te_clust.csv',row.names = FALSE)



#######################
#######################
####################### LASSO ########
#######################


set.seed(542)

train <- read.csv("c:/users/whuang67/downloads/train.csv")
test <- read.csv("c:/users/whuang67/downloads/test.csv")
library(glmnet)
getMAE <- function(actual, predicted){
  mean(abs(actual - predicted))
}

## Test###############################################


levels.col <- base::array(NA, dim = 117) ###
levels.col.logis <- base::array(NA, dim = 117)
for (i in 1:117){
  levels.col[i] <- base::length(base::table(train[, i]))
  if (levels.col[i] <= 10){
    levels.col.logis[i] <- TRUE
  } else {
    levels.col.logis[i] <- FALSE
  }
}


length(base::c(base::as.logical(levels.col.logis), base::as.logical(rep(TRUE, 15))))
train1 <- train[, base::c(base::as.logical(levels.col.logis),
                          base::as.logical(rep(TRUE, 15)))]

library(Matrix)
train.Predictors <- base::data.frame(
  base::as.matrix(
    Matrix::sparse.model.matrix(loss ~ .-1, data=train1)[, -1]
  )
)

train.Response <- base::data.frame(train[, 132])
names(train.Response) <- "loss"

#### Cross-validation to find lambda
#### MAE will be considered here but not the only metric

foldid <- sample(rep(seq(5),
                     length = nrow(train)))
MAE_FindLambda <- array(NA, dim = c(50, 5))

for(i in 1:5){
  FindLambda <- glmnet::glmnet(
    x = as.matrix(train.Predictors[-which(foldid == i), ]),
    y = log(train.Response$loss[-which(foldid == i)]),
    alpha = 1,
    family = "gaussian",
    standardize = FALSE,
    lambda = seq(0.001, 0.05, by = 0.001)
  )
  
  for (j in 1:50){
    predict_value <- predict.glmnet(
      FindLambda,
      newx = as.matrix(train.Predictors[which(foldid == i), ]),
      s = FindLambda$lambda[j]
    )
    MAE_FindLambda[j, i] <- getMAE(train.Response$loss[which(foldid == i)], 
                                   exp(predict_value))
  }
}

GetTrainMAE <- glmnet::glmnet(
  x = as.matrix(train.Predictors),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda =0.05 #FindLambda$lambda
)
GetTrainMAE$beta
TrainMAE <- array(NA, dim = 50)
for (j in 1:50){
  predict_value <- predict.glmnet(
    FindLambda,
    newx = as.matrix(train.Predictors),
    s = FindLambda$lambda[j]
  )
  TrainMAE[j] <- getMAE(train.Response$loss,
                        exp(predict_value))
}

CV_MAE <- rowMeans(MAE_FindLambda)
which(CV_MAE == min(CV_MAE))
CV_MAE


  


#########################
#########################
## Lambda = 0.0045 is used to obtain selected variables

GetBeta <- glmnet::glmnet(
  x = as.matrix(train.Predictors),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.0045
)

# names(train.Predictors)[as.logical(GetBeta$beta)]

# getMAE((train.Response$loss), 
#        exp(predict(GetBeta,
#                newx = as.matrix(train.Predictors))))



############## variables from LASSO
CAT.LASSO <- train[, which(
  names(train) %in% c("cat1", "cat2", "cat4", "cat5", "cat6", "cat7",
                      "cat9", "cat10", "cat11", "cat12", "cat13", "cat23",
                      "cat25", "cat26", "cat27", "cat36", "cat37", "cat38",
                      "cat44", "cat49", "cat52", "cat53", "cat57",
                      "cat71", "cat72", "cat73", "cat75", "cat76",
                      "cat79", "cat80", "cat81", "cat82", "cat83",
                      "cat87", "cat95", "cat98"))]

CONT.LASSO <- train[, which(
  names(train) %in% c("cont2", "cont3", "cont7", "cont12", "cont14"))]
names(CONT.LASSO)



############################################ PCA
numeric_data <- train[,118:131]
pr.comp <- stats::prcomp(numeric_data, center = TRUE, scale. = TRUE)

### First 4 are contained
CONT.PCA <- data.frame(pr.comp$x)[,1:4]
CONT.PCA.test <- data.frame(predict(pr.comp, test[,118:131])[,1:4])




MAE  <-   function(data,lev = NULL,model = NULL) { 
  out <- c(defaultSummary(data, lev = NULL, model = NULL))
  MAE <- mean(abs(data$obs - data$pred))
  c(out, MAE = MAE)
}





######### include Cluster, from group mate
CLUSTER <- read.csv("C:/users/whuang67/documents/tr_clust.csv", header=TRUE)
CLUSTER.test <- read.csv("C:/users/whuang67/documents/te_clust.csv", header=TRUE)

######### Sparse matrix for next step, including CLUSTER here.
library(Matrix)
sparse.CAT.LASSO <- base::data.frame(
  as.matrix(
    sparse.model.matrix(~ .-1, data=data.frame(CAT.LASSO, as.factor(CLUSTER$cluster)))[, -1]
  )
)

########### Model with "first 4 PC, categorical variables, cluster"
data.421 <- cbind(CONT.PCA, sparse.CAT.LASSO)
LASSO.MAE <- array(NA, dim = c(30,5))
for(i in 1:5){
  LassoFitting <- glmnet::glmnet(
    x = as.matrix(data.421)[-which(foldid == i), ],
    y = log(train.Response$loss[-which(foldid == i)]),
    alpha = 1,
    family = "gaussian",
    standardize = FALSE,
    lambda = seq(0.001, 0.03, by = 0.001)
  )
  
  for (j in 1:30){
    predict_value <- predict.glmnet(
      LassoFitting,
      newx = as.matrix(data.421)[which(foldid == i), ],
      s = LassoFitting$lambda[j]
    )
    LASSO.MAE[j, i] <- getMAE(train.Response$loss[which(foldid == i)], 
                              exp(predict_value))
  }
}
min(rowMeans(LASSO.MAE))
LassoFitting$lambda[which.min(rowMeans(LASSO.MAE))]
plot421 <- data.frame(LassoFitting$lambda, rowMeans(LASSO.MAE))



LassoFitting421 <- glmnet::glmnet(
  x = as.matrix(data.421),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.002
)

LASSO421 <- exp(predict(LassoFitting421, newx=as.matrix(data.421), s=0.002))

getMAE(exp(predict(LassoFitting421, newx=as.matrix(data.421), s=0.002)),
       train.Response$loss)

CAT.LASSO.test <- test[, which(
  names(test) %in% c("cat1", "cat2", "cat4", "cat5", "cat6", "cat7",
                      "cat9", "cat10", "cat11", "cat12", "cat13", "cat23",
                      "cat25", "cat26", "cat27", "cat36", "cat37", "cat38",
                      "cat44", "cat49", "cat52", "cat53", "cat57",
                      "cat71", "cat72", "cat73", "cat75", "cat76",
                      "cat79", "cat80", "cat81", "cat82", "cat83",
                      "cat87", "cat95", "cat98"))]
sparse.CAT.LASSO.test <- base::data.frame(
  as.matrix(
    sparse.model.matrix( ~ .-1, data=data.frame(CAT.LASSO.test,
                                                as.factor(CLUSTER.test$cluster)))[, -1]
  )
)

CONT.LASSO.test <- test[, which(
  names(test) %in% c("cont2", "cont3", "cont7", "cont12", "cont14"))]
# CONT.PCA.test

data.421.test <- cbind(CONT.PCA.test, sparse.CAT.LASSO.test)
  
testPredict421 <- data.frame(test$id,
                             exp(predict(LassoFitting421,
                                         newx=as.matrix(data.421.test),
                                         s=0.002)))

names(testPredict421) <- c("id", "loss")
row.names(testPredict421) <- NULL
write.csv(testPredict421,
          file = "C:/users/whuang67/downloads/data_421.csv")



########## Model with v"ariables from LASSO, cluster"
data.422 <- cbind(CONT.LASSO, sparse.CAT.LASSO)
LASSO.MAE <- array(NA, dim = c(30,5))
for(i in 1:5){
  LassoFitting <- glmnet::glmnet(
    x = as.matrix(data.422)[-which(foldid == i), ],
    y = log(train.Response$loss[-which(foldid == i)]),
    alpha = 1,
    family = "gaussian",
    standardize = FALSE,
    lambda = seq(0.001, 0.03, by = 0.001)
  )
  
  for (j in 1:30){
    predict_value <- predict.glmnet(
      LassoFitting,
      newx = as.matrix(data.422)[which(foldid == i), ],
      s = LassoFitting$lambda[j]
    )
    LASSO.MAE[j, i] <- getMAE(train.Response$loss[which(foldid == i)], 
                              exp(predict_value))
  }
}

min(rowMeans(LASSO.MAE))
LassoFitting$lambda[which.min(rowMeans(LASSO.MAE))]
plot422 <- data.frame(LassoFitting$lambda, rowMeans(LASSO.MAE))


LassoFitting422 <- glmnet::glmnet(
  x = as.matrix(data.422),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.002
)

LASSO422 <- exp(predict(LassoFitting422, newx=as.matrix(data.422), s=0.002))
getMAE(exp(predict(LassoFitting422, newx=as.matrix(data.422), s=0.002)),train.Response$loss)


data.422.test <- cbind(CONT.LASSO.test, sparse.CAT.LASSO.test)

testPredict422 <- data.frame(test$id,
                             exp(predict(LassoFitting422,
                                         newx=as.matrix(data.422.test),
                                         s=0.002)))

names(testPredict422) <- c("id", "loss")
row.names(testPredict422) <- NULL
write.csv(testPredict422,
          file = "C:/users/whuang67/downloads/data_422.csv")



############# Variables from TREE
CAT.TREE <- train[, which(
  names(train) %in% c("cat80", "cat79", "cat87", "cat57", "cat101", 
                       "cat100", "cat12", "cat81",  "cat89",
                       "cat108", "cat7", "cat107", "cat115", "cat105",
                      "cat10", "cat104", "cat114", "cat103", "cat111", "cat1",
                      "cat72", "cat94",  "cat53", "cat106", 
                       "cat44", "cat61",  "cat2"))]



CONT.TREE <- train[, which(
  names(train) %in% c("cont2","cont7","cont12","cont11",
                      "cont4","cont8","cont13","cont6", "cont3"))]



CAT.TREE.test <- test[, which(
  names(test) %in% c("cat80", "cat79", "cat87", "cat57", "cat101", 
                      "cat100", "cat12", "cat81",  "cat89",
                      "cat108", "cat7", "cat107", "cat115", "cat105",
                      "cat10", "cat104", "cat114", "cat103", "cat111", "cat1",
                      "cat72", "cat94",  "cat53", "cat106", 
                      "cat44", "cat61",  "cat2"))]

CONT.TREE.test <- test[, which(
  names(test) %in% c("cont2","cont7","cont12","cont11",
                      "cont4","cont8","cont13","cont6", "cont3"))]

######## train and test set contain different levels in particular variables, fix this issue
CAT.TREE.total <- rbind(CAT.TREE, CAT.TREE.test)
CLUSTER.total <- rbind(CLUSTER, CLUSTER.test)

sparse.CAT.TREE.total <- data.frame(as.matrix(
    sparse.model.matrix( ~ .-1, data=data.frame(CAT.TREE.total,
                                                as.factor(CLUSTER.total$cluster)))[, -1]
))

### put concatenated back to train and test
sparse.CAT.TREE <- sparse.CAT.TREE.total[1:188318, ]
sparse.CAT.TREE.test <- sparse.CAT.TREE.total[188319:313864, ]

###### model with "all variables from TREE, cluster"

data.423 <- cbind(CONT.TREE, sparse.CAT.TREE)
LASSO.MAE <- array(NA, dim = c(30,5))
for(i in 1:5){
  LassoFitting <- glmnet::glmnet(
    x = as.matrix(data.423)[-which(foldid == i), ],
    y = log(train.Response$loss[-which(foldid == i)]),
    alpha = 1,
    family = "gaussian",
    standardize = FALSE,
    lambda = seq(0.001, 0.03, by = 0.001)
  )
  
  for (j in 1:30){
    predict_value <- predict.glmnet(
      LassoFitting,
      newx = as.matrix(data.423)[which(foldid == i), ],
      s = LassoFitting$lambda[j]
    )
    LASSO.MAE[j, i] <- getMAE(train.Response$loss[which(foldid == i)], 
                              exp(predict_value))
  }
}

min(rowMeans(LASSO.MAE))
LassoFitting$lambda[which.min(rowMeans(LASSO.MAE))]
plot423 <- data.frame(LassoFitting$lambda, rowMeans(LASSO.MAE))


LassoFitting423 <- glmnet::glmnet(
  x = as.matrix(data.423),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.001
)
LASSO423 <- exp(predict(LassoFitting423, newx=as.matrix(data.423), s=0.001))
getMAE(exp(predict(LassoFitting423, newx=as.matrix(data.423), s=0.001)),
       train.Response$loss)

data.423.test <- cbind(CONT.TREE.test, sparse.CAT.TREE.test)

testPredict423 <- data.frame(test$id,
                             exp(predict(LassoFitting423,
                                         newx=as.matrix(data.423.test),
                                         s=0.001)))


names(testPredict423) <- c("id", "loss")
row.names(testPredict423) <- NULL
write.csv(testPredict423,
          file = "C:/users/whuang67/downloads/data_423.csv")




##### Model with "4 PC, categorical variables from TREE, cluster"
data.424 <- cbind(CONT.PCA, sparse.CAT.TREE)
LASSO.MAE <- array(NA, dim = c(30,5))
for(i in 1:5){
  LassoFitting <- glmnet::glmnet(
    x = as.matrix(data.424)[-which(foldid == i), ],
    y = log(train.Response$loss[-which(foldid == i)]),
    alpha = 1,
    family = "gaussian",
    standardize = FALSE,
    lambda = seq(0.001, 0.03, by = 0.001)
  )
  
  for (j in 1:30){
    predict_value <- predict.glmnet(
      LassoFitting,
      newx = as.matrix(data.424)[which(foldid == i), ],
      s = LassoFitting$lambda[j]
    )
    LASSO.MAE[j, i] <- getMAE(train.Response$loss[which(foldid == i)], 
                              exp(predict_value))
  }
}

min(rowMeans(LASSO.MAE))
LassoFitting$lambda[which.min(rowMeans(LASSO.MAE))]
plot424 <- data.frame(LassoFitting$lambda, rowMeans(LASSO.MAE))


LassoFitting424 <- glmnet::glmnet(
  x = as.matrix(data.424),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.001
)

LASSO424 <- exp(predict(LassoFitting424, newx=as.matrix(data.424), s=0.02))
getMAE(exp(predict(LassoFitting424, newx=as.matrix(data.424), s=0.02)),train.Response$loss)


data.424.test <- cbind(CONT.PCA.test, sparse.CAT.TREE.test)

testPredict424 <- data.frame(test$id,
                             exp(predict(LassoFitting424,
                                         newx=as.matrix(data.424.test),
                                         s=0.001)))


names(testPredict424) <- c("id", "loss")
row.names(testPredict424) <- NULL
write.csv(testPredict424,
          file = "C:/users/whuang67/downloads/data_424.csv")




####### Model 5 with "categorical LASSO, 5 SAVE, and cluster"

install.packages("glmnet")
?install.packages


trainingdata5 <- trainingdata[[5]]
library(glmnet)

set.seed(542)
foldid <- sample(rep(seq(5),
                     length = nrow(train)))

train.Response <- data.frame(train[,132])
names(train.Response) <- "loss"
data.425 <- trainingdata5

LASSO.MAE <- array(NA, dim = c(30,5))

for(i in 1:5){
  LassoFitting <- glmnet::glmnet(
    x = as.matrix(data.425)[-which(foldid == i), ],
    y = log(train.Response$loss[-which(foldid == i)]),
    alpha = 1,
    family = "gaussian",
    standardize = FALSE,
    lambda = seq(0.001, 0.03, by = 0.001)
  )
  
  for (j in 1:30){
    predict_value <- predict.glmnet(
      LassoFitting,
      newx = as.matrix(data.425)[which(foldid == i), ],
      s = LassoFitting$lambda[j]
    )
    LASSO.MAE[j, i] <- getMAE(train.Response$loss[which(foldid == i)], 
                              exp(predict_value))
  }
}

min(rowMeans(LASSO.MAE))
LassoFitting$lambda[which.min(rowMeans(LASSO.MAE))]
plot425 <- data.frame(LassoFitting$lambda, rowMeans(LASSO.MAE))


LassoFitting425 <- glmnet::glmnet(
  x = as.matrix(data.425),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.002
)

LASSO425 <- exp(predict(LassoFitting425, newx=as.matrix(data.425), s=0.002))
getMAE(exp(predict(LassoFitting425, newx=as.matrix(data.425), s=0.002)),train.Response$loss)



data.425.test <- testingdata[[5]]


testPredict425 <- data.frame(exp(predict(LassoFitting425,
                                         newx=as.matrix(data.425.test),
                                         s=0.002)))


names(testPredict425) <- "loss"
row.names(testPredict425) <- NULL
write.csv(testPredict425,
          file = "C:/users/whuang67/downloads/data_425.csv")



######### Model 6 "Tree categorical variables, 5 SAVE, cluster"

trainingdata6 <- trainingdata[[6]]
data.426 <- trainingdata6
LASSO.MAE <- array(NA, dim = c(30,5))
for(i in 1:5){
  LassoFitting <- glmnet::glmnet(
    x = as.matrix(data.426)[-which(foldid == i), ],
    y = log(train.Response$loss[-which(foldid == i)]),
    alpha = 1,
    family = "gaussian",
    standardize = FALSE,
    lambda = seq(0.001, 0.03, by = 0.001)
  )
  
  for (j in 1:30){
    predict_value <- predict.glmnet(
      LassoFitting,
      newx = as.matrix(data.426)[which(foldid == i), ],
      s = LassoFitting$lambda[j]
    )
    LASSO.MAE[j, i] <- getMAE(train.Response$loss[which(foldid == i)], 
                              exp(predict_value))
  }
}


min(rowMeans(LASSO.MAE))
LassoFitting$lambda[which.min(rowMeans(LASSO.MAE))]
plot426 <- data.frame(LassoFitting$lambda, rowMeans(LASSO.MAE))



LassoFitting426 <- glmnet::glmnet(
  x = as.matrix(data.426),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.001
)

LASSO426 <- exp(predict(LassoFitting426, newx=as.matrix(data.426), s=0.001))
getMAE(exp(predict(LassoFitting426, newx=as.matrix(data.426), s=0.001)),train.Response$loss)

data.426.test <- testingdata[[6]]

testPredict426 <- data.frame(exp(predict(LassoFitting426,
                                         newx=as.matrix(data.426.test),
                                         s=0.001)))


################ Visualize
library(ggplot2)
ggplot() +
  geom_line(data = plot421,
            mapping = aes(x = LassoFitting.lambda,
                          y = rowMeans.LASSO.MAE.,
                          color = "CAT(LASSO), PC & Cluster")) + 
  geom_line(data = plot422,
            mapping = aes(x = LassoFitting.lambda,
                          y = rowMeans.LASSO.MAE.,
                          color = "Variable(LASSO) & Cluster")) + 
  geom_line(data = plot423,
            mapping = aes(x = LassoFitting.lambda,
                          y = rowMeans.LASSO.MAE.,
                          color = "Variable(Tree) & Cluster")) +
  geom_line(data = plot424,
            mapping = aes(x = LassoFitting.lambda,
                          y = rowMeans.LASSO.MAE.,
                          color = "CAT(Tree), PC & Cluster")) +
  xlab("Lambda") + 
  ylab("Cross-validation MAE") +
  ggtitle("Lambda selection of LASSO") +
  labs(colour = "Models")





#variables selection by lasso
levels.col.logis <- base::array(NA, dim = 117)
for (i in 1:117){
  levels.col[i] <- base::length(base::table(train[, i]))
  if (levels.col[i] <= 10){
    levels.col.logis[i] <- TRUE
  } else {
    levels.col.logis[i] <- FALSE
  }
}


length(base::c(base::as.logical(levels.col.logis), base::as.logical(rep(TRUE, 15))))
train1 <- train[, base::c(base::as.logical(levels.col.logis),
                          base::as.logical(rep(TRUE, 15)))]

library(Matrix)
train.Predictors <- base::data.frame(
  base::as.matrix(
    Matrix::sparse.model.matrix(loss ~ .-1, data=train1)[, -1]
  )
)


train.Response <- base::data.frame(train[, 132])
names(train.Response) <- "loss"

GetBeta <- glmnet::glmnet(
  x = as.matrix(train.Predictors),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.0155
)
colnames(train.Predictors)[which(GetBeta$beta!=0)]

Lasso<-train[lasso_index]

#variables from lasso and pca



#PCA of continous variables
numeric_data<- train[,118:131]
pr.comp=prcomp(numeric_data,center = TRUE,scale. = TRUE)
pr.var=pr.comp$sdev^2
pve=pr.var/sum(pr.var)
pve
summary(pr.comp)
ggplot()+geom_line(aes(x=(1:length(pve)),y=pve),size=1,col="green")


#SAVE(slice inversed regression, dimension reduce for regression)
fit.sir = dr(loss~., data = data.frame(numeric_data,loss=train[,132]), method = "save", nslices = 10000)
fit.sir$evalues/sum(fit.sir$evalues)
#first 9 direction of numeric variable that can explain 70% variance of Y
SAVE<-as.matrix(numeric_data)%*%t(fit.sir$evectors)
SIR_9<-as.matrix(numeric_data)%*%t(fit.sir$evectors)[,1:9]

#split data into train data and validation data
#set.seed(123)
#index<-c(1:nrow(train))
#train_index<-sample(1:length(train[,132]),0.8*length(train[,132]))
#validation_index<-index[which(c(1:length(train[,132]))%in%train_index=='FALSE')]
#traindata<-train[train_index,]
#validation<-train[validation_index,]

#variables from PCA and rf
#numeric_traindata<-traindata[,118:131]
#str(numeric_traindata)
PCA_4<-as.data.frame(as.matrix(numeric_data)%*%pr.comp$rotation[,1:4])
cat_rf<-train[as.character(average$variables)[grep("cat",average$variables)]]
#cat_rf<-traindata[candidator][sapply(train[candidator],class)=="character"]
cat_rf<-as.data.frame(sapply(cat_rf,as.factor))
PCA_RF<-cbind(PCA_3,cat_rf)


#variables from SAVE and rf
SAVE_RF<-cbind(SIR_9,train[as.character(average$variables)[grep("cat",average$variables)[1:11]]])

#variables from rf
RF<-train[as.character(average$variables)[1:20]]


#variables from lasso and pca
GetBeta <- glmnet::glmnet(
  x = as.matrix(train.Predictors),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.015
)
colnames(train.Predictors)[which(GetBeta$beta!=0)]

Lasso_pca<-cbind(PCA_3,train[c("cat1","cat2","cat4","cat5","cat10","cat12","cat25","cat26",
                               "cat38","cat44","cat53","cat72","cat73","cat79","cat80","cat81",
                               "cat94","cat95")])


#variables from lasso and save
GetBeta <- glmnet::glmnet(
  x = as.matrix(train.Predictors),
  y = log(train.Response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.028)
colnames(train.Predictors)[which(GetBeta$beta!=0)]

Lasso_save<-cbind(SIR_9,train[c("cat1","cat2","cat4","cat5","cat10","cat12","cat72","cat79","cat80","cat81",
                                "cat94")])



lasso_index<-c("cat1", "cat2", "cat4", "cat5", "cat6", "cat7",
               "cat9", "cat10", "cat11", "cat12", "cat13", "cat23",
               "cat25", "cat26", "cat27", "cat36", "cat37", "cat38",
               "cat44", "cat49", "cat52", "cat53", "cat57",
               "cat71", "cat72", "cat73", "cat75", "cat76",
               "cat79", "cat80", "cat81", "cat82", "cat83",
               "cat87", "cat95", "cat98","cont2", "cont3", "cont7", "cont12", "cont14")

lasso_cat_index<-c("cat1", "cat2", "cat4", "cat5", "cat6", "cat7",
                   "cat9", "cat10", "cat11", "cat12", "cat13", "cat23",
                   "cat25", "cat26", "cat27", "cat36", "cat37", "cat38",
                   "cat44", "cat49", "cat52", "cat53", "cat57",
                   "cat71", "cat72", "cat73", "cat75", "cat76",
                   "cat79", "cat80", "cat81", "cat82", "cat83",
                   "cat87", "cat95", "cat98")

RF_index<-as.character(average$variables)

RF_cat_index<-colnames(cat_rf)

SAVE_loading<-fit.sir$evectors[,1:5]




#MAE metric
MAE<-function (data,lev = NULL,model = NULL) { 
  out <- c(defaultSummary(data, lev = NULL, model = NULL))
  MAE <- mean(abs(data$obs - data$pred))
  c(out,MAE=MAE)
}

#dependent variable Y
Y<-train[,132]


#6 different variable set
#1. lasso set  2.rf set 3.lasso-pca set 4.rf-pca set 5.lasso-save set 6.rf-save set
predictorset<-list()
predictorset[[1]]<-Lasso
predictorset[[2]]<-RF
predictorset[[3]]<-Lasso_pca
predictorset[[4]]<-PCA_RF
predictorset[[5]]<-Lasso_save
predictorset[[6]]<-SAVE_RF


#lasso variables
CAT.TREE.total_1 <- rbind(cbind(train[lasso_index],clust=as.factor(tr_clust$cluster)),cbind(test[lasso_index],clust=as.factor(te_clust$cluster)))
sparse.CAT.TREE.total_1 <- data.frame(as.matrix(
  sparse.model.matrix( ~ .-1, data=data.frame(CAT.TREE.total_1))[, -1]
))
Lasso_trainingset<- sparse.CAT.TREE.total_1[1:188318, ]
lasso_testdata <- sparse.CAT.TREE.total_1[188319:313864, ]

#Rf varibles
CAT.TREE.total_1 <- rbind(cbind(train[RF_index],clust=as.factor(tr_clust$cluster)),cbind(test[,RF_index],clust=as.factor(te_clust$cluster)))
sparse.CAT.TREE.total_1 <- data.frame(as.matrix(
  sparse.model.matrix( ~ .-1, data=data.frame(CAT.TREE.total_1))[, -1]
))
rf_trainingset<- sparse.CAT.TREE.total_1[1:188318, ]
rf_testdata <- sparse.CAT.TREE.total_1[188319:313864, ]
dim(rf_trainingset)
dim(rf_testdata)
#lasso+PCA
CAT.TREE.total_1 <- rbind(cbind(train[lasso_cat_index],clust=as.factor(tr_clust$cluster)),cbind(test[lasso_cat_index],clust=as.factor(te_clust$cluster)))

sparse.CAT.TREE.total_1 <- data.frame(as.matrix(
  sparse.model.matrix( ~ .-1, data=data.frame(CAT.TREE.total_1))[, -1]
))
Lassopca_trainingset<- cbind(sparse.CAT.TREE.total_1[1:188318, ],PCA_4)
PCA_test_4<-as.data.frame(as.matrix(test[,118:131])%*%pr.comp$rotation[,1:4])
lassopca_testdata <- cbind(sparse.CAT.TREE.total_1[188319:313864,],PCA_test_4)
dim(Lassopca_trainingset)
dim(lassopca_testdata)
#rf+PCA
CAT.TREE.total_1 <- rbind(cbind(train[RF_cat_index],clust=as.factor(tr_clust$cluster)),cbind(test[RF_cat_index],clust=as.factor(te_clust$cluster)))
sparse.CAT.TREE.total_1 <- data.frame(as.matrix(
  sparse.model.matrix( ~ .-1, data=data.frame(CAT.TREE.total_1))[, -1]
))
rfpca_trainingset<- cbind(sparse.CAT.TREE.total_1[1:188318,],PCA_4)
rfpca_testdata <- cbind(sparse.CAT.TREE.total_1[188319:313864,],PCA_test_4)

#lasso+save
CAT.TREE.total_1 <- rbind(cbind(train[lasso_cat_index],clust=as.factor(tr_clust$cluster)),cbind(test[lasso_cat_index],clust=as.factor(te_clust$cluster)))
sparse.CAT.TREE.total_1 <- data.frame(as.matrix(
  sparse.model.matrix( ~ .-1, data=data.frame(CAT.TREE.total_1))[, -1]
))

fit.sir = dr(loss~., data = data.frame(numeric_data,loss=train[,132]), method = "save", nslices = 10000)
SAVE<-as.matrix(numeric_data)%*%t(fit.sir$evectors)
SIR_train_5<-as.matrix(train[,118:131])%*%t(fit.sir$evectors)[,1:5]
SIR_test_5<-as.matrix(test[,118:131])%*%t(fit.sir$evectors)[,1:5]

Lassosir_trainingset<- cbind(sparse.CAT.TREE.total_1[1:188318, ],SIR_train_5)

lassosir_testdata <- cbind(sparse.CAT.TREE.total_1[188319:313864,],SIR_test_5)
dim(Lassosir_trainingset)
dim(lassosir_testdata)

#rf+save
CAT.TREE.total_1 <- rbind(cbind(train[RF_cat_index],clust=as.factor(tr_clust$cluster)),cbind(test[RF_cat_index],clust=as.factor(te_clust$cluster)))
sparse.CAT.TREE.total_1<- data.frame(as.matrix(   
  sparse.model.matrix( ~ .-1, data=data.frame(CAT.TREE.total_1))[, -1]
))

fit.sir = dr(loss~., data = data.frame(numeric_data,loss=train[,132]), method = "save", nslices = 10000)
SAVE<-as.matrix(numeric_data)%*%t(fit.sir$evectors)
SIR_train_5<-as.matrix(train[,118:131])%*%t(fit.sir$evectors)[,1:5]
SIR_test_5<-as.matrix(test[,118:131])%*%t(fit.sir$evectors)[,1:5]

rfsir_trainingset<- cbind(sparse.CAT.TREE.total_1[1:188318, ],SIR_train_5)

rfsir_testdata <- cbind(sparse.CAT.TREE.total_1[188319:313864,],SIR_test_5)
dim(rfsir_trainingset)
dim(rfsir_testdata)


#test

options( java.parameters = "-Xmx8g" )
library(extraTrees)
cvControl <- trainControl(method = "none",summaryFunction = MAE)
tunegrid<-expand.grid(mtry=c(floor(ncol(rf_trainingset)/3)),numRandomCuts=10)
randomftest<- train(log(loss)~.,
                    data=as.data.frame(cbind(rf_trainingset,Y)),
                    method = "extraTrees",
                    metric= "MAE",
                    trControl = cvControl,
                    tuneGrid = tunegrid,
                    ntree=10,nodesize=5)
exp(predict(randomftest,rf_trainingset))
predict(randomftest,rf_testdata)


cvControl <- trainControl(method = "none",verbose = TRUE,summaryFunction = MAE)
xgbGrid <-  expand.grid  (nrounds=c(10), 
                          max_depth=c(3), 
                          eta=c(0.01),
                          gamma= c(1),
                          colsample_bytree=c(0.8),
                          min_child_weight=c(1),
                          subsample=1)
xgbtest <- train (loss~.,
                  data=as.data.frame(cbind(rfsir_trainingset,Y)),
                  method = "xgbTree",
                  trControl = cvControl,
                  verbose = TRUE,
                  objective= "reg:linear",
                  metric= "MAE",
                  maximize=FALSE,
                  tuneGrid = xgbGrid)
predict(xgbtest,rfsir_trainingset)
predict(xgbtest,rfsir_testdata)



#Sparse.model.matric
#library("Matrix")
install.packages("extraTrees")
options( java.parameters = "-Xmx8g" )
library(extraTrees)



#Random Forest(low bias high variance)
randomf1<-list()
randomf2<-list()
for(i in 1:6)
{
  cvControl <- trainControl(method = "none",summaryFunction = MAE)
  tunegrid<-expand.grid(mtry=c(floor(ncol(Lasso_trainingset)/3)),numRandomCuts=10)
  randomf11<- train(log(loss)~.,
                    data=as.data.frame(cbind(Lasso_trainingset,Y)),
                    method = "extraTrees",
                    metric= "MAE",
                    trControl = cvControl,
                    tuneGrid = tunegrid,
                    ntree=10,nodesize=5
  )
}
#randomForest(loss~.,data=cbind(PCA_RF[1:100,1:4],Y[1:100,]),mtry=1,ntree=100)

#a<-as.matrix(sparse.model.matrix(Y ~ ., data=cbind(PCA_RF,Y))[, -1])
#a<-extraTrees(a,Y,ntree = 500,mtry=floor(ncol(PCA_RF)/3),numRandomCuts=10,
#          numThreads=3)

for(i in 1:6)
{
  #Random Forest(high bias low variance)
  cvControl <- trainControl(method = "none",summaryFunction = MAE)
  tunegrid<-expand.grid(mtry=c(floor(ncol(predictorset[[i]])/3)),numRandomCuts=10)
  randomf2[[i]]<- train(loss~.,
                        data=cbind(predictorset[[i]],Y),
                        method = "extraTrees",
                        trControl = cvControl,
                        metric= "MAE",
                        tuneGrid = tunegrid,
                        ntree=1600,nodesize=20)
}

xgb1<-list()
xgb2<-list()
for(i in 1:6)
{
  #xgboost(low bias high variance)
  cvControl <- trainControl(method = "none",verbose = TRUE,summaryFunction = MAE)
  xgbGrid <-  expand.grid  (nrounds=c(1000), 
                            max_depth=c(10), 
                            eta=c(0.01),
                            gamma= c(1),
                            colsample_bytree=c(0.8),
                            min_child_weight=c(1),
                            subsample=1)
  xgb1[[i]] <- train (loss~.,
                      data=cbind(predictorset[[i]],Y),
                      method = "xgbTree",
                      trControl = cvControl,
                      verbose = TRUE,
                      objective= "reg:linear",
                      metric= "MAE",
                      maximize=FALSE,
                      tuneGrid = xgbGrid)
  
  
  #xgboost(high bias low variance)
  cvControl <- trainControl(method = "none",verbose = TRUE,summaryFunction = MAE)
  xgbGrid <-  expand.grid  (nrounds=c(200), 
                            max_depth=c(10), 
                            eta=c(0.5),
                            gamma= c(1),
                            colsample_bytree=c(0.8),
                            min_child_weight=c(1),
                            subsample=1)
  xgb2[[i]] <- train (loss~.,
                      data=cbind(predictorset[[i]],Y),
                      method = "xgbTree",
                      trControl = cvControl,
                      verbose = TRUE,
                      objective= "reg:linear",
                      metric= "MAE",
                      maximize=FALSE,
                      tuneGrid = xgbGrid)
  
}


#Linear SVM
cvControl <- trainControl(method = "none",
                          number = 3,
                          verbose = TRUE,
                          
                          summaryFunction = MAE)

)



#lasso ensemble
log(Yhat)
log(Y_test)
cvControl <- trainControl(method = "cv",number=10,summaryFunction = MAE)
lassoGrid <-  expand.grid(alpha=c(1),lambda=c(seq(0,4,0.2)))
lassoens <- train (log(loss)~.,
                   data=cbind(log(Yhat),Y),
                   method = 'glmnet_h2o',
                   trControl = cvControl,
                   
                   
                   metric= "MAE",
                   maximize=FALSE,
                   tuneGrid = lassoGrid)
lassoen_training<-exp(predicti(lassoens,log(Yhat)))
lassoen_training_error<-mean(data.matrix(abs(lassoen_training-Y)))
lassoen_test<-exp(predicti(lassoens,log(Y_test)))


#GBM ensemble
cvControl <- trainControl(method = "cv",number=10,summaryFunction = MAE)
gbmGrid <-  expand.grid(ntree=c(1:5)*100, max_depth=c(4),min_rows=c(20),learn_rate=c(0.01,0.05,0.1),col_sample_rate=c(0.8))
gbmens <- train (log(loss)~.,
                 data=cbind(log(Yhat),Y),
                 method = 'gbm_h2o',
                 trControl = cvControl,
                 metric= "MAE",
                 maximize=FALSE,
                 tuneGrid = gbmGrid)
gbmen_training<-exp(predicti(gbmens,log(Yhat)))
gbmen_training_error<-mean(data.matrix(abs(gbmen_training-Y)))
gbmen_test<-exp(predicti(gbmens,log(Y_test)))




##############
##############
###########3##  Ensemble


LASSO421
LASSO422
LASSO423
LASSO424
LASSO425
LASSO426

tree1 <- read.csv("c:/users/whuang67/downloads/train_tree11 (1).csv", header=TRUE)
tree2 <- read.csv("c:/users/whuang67/downloads/train_tree2_212.9257 (1).csv", header=TRUE)
tree3 <- read.csv("c:/users/whuang67/downloads/train_tree3 (1).csv", header=TRUE)
tree5 <- read.csv("c:/users/whuang67/downloads/train_tree5 (1).csv", header=TRUE)
tree6 <- read.csv("c:/users/whuang67/downloads/train_tree6 (1).csv", header=TRUE)
tree4 <- read.csv("c:/users/whuang67/downloads/train_tree4.csv", header=TRUE)

load("c:/users/whuang67/downloads/xgbresult.RData")
xgboost11 <- xg1_train[[1]]
xgboost12 <- xg1_train[[2]]
xgboost13 <- xg1_train[[3]]
xgboost14 <- xg1_train[[4]]
xgboost15 <- xg1_train[[5]]
xgboost16 <- xg1_train[[6]]



xgboost21 <- xg2_train[[1]]
xgboost22 <-  xg2_train[[2]]
xgboost23 <-  xg2_train[[3]]
xgboost24 <-  xg2_train[[4]]
xgboost25 <-  xg2_train[[5]]
xgboost26 <-  xg2_train[[6]]


ensemble.predictors <- data.frame(
  LASSO421,
  LASSO422,
  LASSO423,
  LASSO424,
  LASSO425,
  LASSO426,
 ##] tree1[,2],
#  exp(tree2[,2]),
 # tree3[,2],
#  tree4[,2],
#  tree5[,2],
#  tree6[,2],
  xgboost11,
  xgboost12,
  xgboost13,
  xgboost14,
  xgboost15,
  xgboost16,
  xgboost21,
  xgboost22,
  xgboost23,
  xgboost24,
  xgboost25,
  xgboost26
)
write.csv(ensemble.predictors,
          "C:/users/whuang67/downloads/ensemble_predictors.csv")
ensemble.response <- train.Response


tree1.test <- read.csv("c:/users/whuang67/downloads/test_tree1 (1).csv", header=TRUE)
tree2.test <- read.csv("c:/users/whuang67/downloads/test_tree2_1256.04345 (1).csv", header=TRUE)
tree3.test <- read.csv("c:/users/whuang67/downloads/test_tree3 (1).csv", header=TRUE)
tree4.test <- read.csv("c:/users/whuang67/downloads/test_tree4 (1).csv", header=TRUE)
tree5.test <- read.csv("c:/users/whuang67/downloads/test_tree5 (1).csv", header=TRUE)
tree6.test <- read.csv("c:/users/whuang67/downloads/test_tree6 (1).csv", header=TRUE)


ensemble.predictors.test <- data.frame(testPredict421[,2],
                                       testPredict422[,2],
                                       testPredict423[,2],
                                       testPredict424[,2],
                                       testPredict425,
                                       testPredict426,
                                      # tree1.test[,2],
                                      # tree2.test[,2],
                                      # tree3.test[,2],
                                      # tree4.test[,2],
                                      # tree5.test[,2],
                                      # tree6.test[,2],
                                       xg1_test[[1]],
                                       xg1_test[[2]],
                                       xg1_test[[3]],
                                       xg1_test[[4]],
                                       xg1_test[[5]],
                                       xg1_test[[6]],
                                       xg2_test[[1]],
                                       xg2_test[[2]],
                                       xg2_test[[3]],
                                       xg2_test[[4]],
                                       xg2_test[[5]],
                                       xg2_test[[6]])

dim(ensemble.predictors.test)
########### Average
ensemble.average <- rowMeans(ensemble.predictors)
ensemble.average.test <- rowMeans(ensemble.predictors.test)
write.csv(data.frame(test$id, ensemble.average.test),
          "c:/users/whuang67/downloads/ensemble_ave.csv")


dim(ensemble.predictors.test)
############### LASSO




####### Tuning

LASSOensemble.MAE <- array(NA, dim = c(9, 5))
  for(i in 1:5){
    LASSO.ensemble <- glmnet::glmnet(
      x = as.matrix(log(ensemble.predictors))[-which(foldid == i), ],
      y = log(ensemble.response$loss[-which(foldid == i)]),
      alpha = 1,
      family = "gaussian",
      standardize = FALSE,
      lambda = seq(0.1, 0.9, by = 0.1)
    )
    
    for (j in 1:9){
      predict_value <- predict.glmnet(
        LASSO.ensemble,
        newx = as.matrix(log(ensemble.predictors))[which(foldid == i), ],
        s = LASSO.ensemble$lambda[j]
      )
      LASSOensemble.MAE[j, i] <- getMAE(ensemble.response$loss[which(foldid ==i)],
                                        exp(predict_value))
    }
  }


min(rowMeans(LASSOensemble.MAE))
LASSO.ensemble$lambda[which.min(rowMeans((LASSOensemble.MAE)))]




########### Fit with best lambda

LASSO.final <- glmnet(
  x = as.matrix(log(ensemble.predictors)),
  y = log(ensemble.response$loss),
  alpha = 1,
  family = "gaussian",
  standardize = FALSE,
  lambda = 0.1
)

LASSO.ensemble.result <- exp(predict(LASSO.final,
                                     as.matrix(log(ensemble.predictors)),
                                     s = 0.1))
getMAE(ensemble.response$loss, LASSO.ensemble.result)
LASSO.ensemble.test <- exp(predict(LASSO.final,
                                   as.matrix(log(ensemble.predictors.test)),
                                   s = 0.1))
write.csv(data.frame(test$id, LASSO.ensemble.test),
          "c:/users/whuang67/downloads/final_test.csv")



M <- cor(ensemble.predictors)
#col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
#                           "cyan", "#007FFF", "blue","#00007F")) 
corrplot.mixed(M, lower="circle",upper="number",col=c("green","blue"))
corrplot(ensemble.predictors)
