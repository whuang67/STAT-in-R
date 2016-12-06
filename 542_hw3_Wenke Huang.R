## Question 1

# loading data
set.seed(542)
library(ElemStatLearn)
SouthAfrica <- SAheart

#linear SVM
library(kernlab)
library(lattice)
library(ggplot2)
library(caret)
svmControl <- trainControl(method="cv", number=5, returnResamp="all",
                           verbose=FALSE)
model.svmLinear3 <- train(
  as.factor(chd) ~.,
  data = SouthAfrica,
  method = "svmLinear",
  trContorl = svmControl,
  tuneGrid = expand.grid(C = c(2^(-2:2)))
)

library(knitr)
kable(data.frame(model.svmLinear3$results[,(1:2)]),
      align = "c",
      caption = "Tuning Parameters and Accuracies (Linear SVM)")

# Radial SVM
set.seed(542)
RadialGrid <- expand.grid(C = c(2^(-2:2)), sigma=c(2^(-3:1)))
model.svmRadial3 <- train(
  as.factor(chd) ~.,
  data = SouthAfrica,
  method = "svmRadial",
  trContorl = svmControl,
  tuneGrid = RadialGrid
)

kable(data.frame(model.svmRadial3$results)[(1:3)],
      align = "c",
      caption = "Tuning Parameters and Accuracies (Radial SVM)")

# Polynomial SVM
PolyGrid <- expand.grid(C = c(2^(-2:2)), degree=c(1:3), scale=1)
model.svmPoly3 <- train(
  as.factor(chd) ~.,
  data = SouthAfrica,
  method = "svmPoly",
  trContorl = svmControl,
  tuneGrid = PolyGrid
)

kable(data.frame(model.svmPoly3$results)[c(1,2,4)],
      align = "c",
      caption = "Tuning Parameters and Accuracies (Polynomial SVM)")

# Comparation
kable(data.frame(Accuracy.Linear = max(model.svmLinear3$results$Accuracy),
                 Accuracy.Radial = max(model.svmRadial3$results$Accuracy),
                 Accuracy.Polynomial = max(model.svmPoly3$results$Accuracy)),
      align = "c",
      caption = "Best accuracies of models above")

kable(data.frame(model.svmPoly3$bestTune[c(1,3)], Accuracy = max(model.svmPoly3$results$Accuracy)),
      align = "c",
      caption = "Final choose of this model")



## Question 3
# a
set.seed(542)
n <- 200
p <- 4
X <- array(data=0, dim=c(n,p))
for(i in 1:p){
  X[,i] <- rnorm(n, mean=0, sd =1)
}

# b
##### Y_mean = X1+X2+X3+2*X4
beta <- as.matrix(c(1,1,1,2))
Y_mean <- as.matrix(X) %*% beta

# c
Y <- Y_mean + rnorm(n, mean=0, sd=1)

# d
set.seed(542)
library(randomForest)
model.rf <- randomForest(
  data.frame(X),
  Y,
  ntree = 500,
  mtry = 2,
  nodesize = 25,
  importance = FALSE
)
Y_pred <- predict(model.rf, newdata = X)

# e
Cov <- array(data = 0, dim = 10)
for (i in 1:10){
  Y.repeat <- Y_mean + rnorm(n, mean=0, sd=1)
  model.rf <- randomForest(
    X, Y.repeat, ntree = 500, mtry = 2, nodesize = 25
  )
  Y_pred.repeat <- predict(model.rf, newdata = X)
  Cov[i] <- cov(Y.repeat, Y_pred.repeat)
}

# f
df <- sum(Cov)/1

FixedTune.table <- kable(
  data.frame(Parameter = "Value",
             ntree = 500,
             mtry = 2,
             nodesize = 25,
             DoF = df),
  align="c",
  caption="Fixed Tuning")
FixedTune.table


# g
set.seed(542)
Cov.varyTune <- array(data = 0, dim = 10)
df.varyTune <- array(data = 0, dim = c(3,3,3))
for (i in 1:3){
  for (j in 1:3){
    for (k in 1:3){
      for (l in 1:10){
        Y.varyTune <- Y_mean + rnorm(n, mean=0, sd=1)
        model.repeat <- randomForest(
          X, Y.varyTune, ntree = 500*i, mtry = j, nodesize = 25*k
        )
        Y_pred.varyTune <- predict(
          model.repeat,
          newdata = X
        )
        Cov.varyTune[l] <- cov(Y.varyTune, Y_pred.varyTune)
      }
      df.varyTune[i,j,k] <- sum(Cov.varyTune)/1
    }
  }
}

# Comparation
Nodesize1 <- data.frame(df.varyTune[,,1],
                        row.names = c("ntree=500", "ntree=1000", "ntree=1500"))
colnames(Nodesize1) <- c(c("mtry=1", "mtry=2", "mtry=3"))
kable(Nodesize1, align="c", caption="Vary Tuning(nodesize=25)")

Nodesize2 <- data.frame(df.varyTune[,,2],
                        row.names = c("ntree=500", "ntree=1000", "ntree=1500"))
colnames(Nodesize2) <- c(c("mtry=1", "mtry=2", "mtry=3"))
kable(Nodesize2, align="c", caption="Vary Tuning(nodesize=50)")

Nodesize3 <- data.frame(df.varyTune[,,3],
                        row.names = c("ntree=500", "ntree=1000", "ntree=1500"))
colnames(Nodesize3) <- c(c("mtry=1", "mtry=2", "mtry=3"))
kable(Nodesize3, align="c", caption="Vary Tuning(nodesize=75)")

