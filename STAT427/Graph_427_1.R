
kran <- read.csv("C:/users/whuang67/downloads/Cleaned_Ticket Redemption Data.csv",
                 header = TRUE)


library(ggplot2)


######################################################################


a <- ggplot()
for (i in levels(kran$Theatre)){
  a <- a + geom_bar(data = kran[which(kran$Theatre == i), ],
                mapping = aes(x = Theatre,
                              fill = as.factor(Redeemed)),
                width = 1,
                color = "black") +
    ylab("Count") + xlab("Theatre")
}
a


b <- ggplot()
for (i in levels(kran$Producer)){
  b <- b + geom_bar(data = kran[which(kran$Producer == i), ],
                    mapping = aes(x = Producer,
                                  fill = as.factor(Redeemed)),
                    width = 1,
                    color = "black") +
    ylab("Count") + xlab("Producer")
}
b

c <- ggplot()
for (i in levels(kran$Disposition)){
  c <- c + geom_bar(data = kran[which(kran$Disposition == i), ],
                    mapping = aes(x = Disposition,
                                  fill = as.factor(Redeemed)),
                    width = 1,
                    color = "black") +
    ylab("Count") + xlab("Disposition")
}
c

d <- ggplot()
for (i in levels(kran$PurchasedOnline)){
  d <- d + geom_bar(data = kran[which(kran$PurchasedOnline == i), ],
                    mapping = aes(x = PurchasedOnline,
                                  fill = as.factor(Redeemed)),
                    width = 1,
                    color = "black") +
    ylab("Count") + xlab("Purchased Online or not")
}
d

e <- ggplot()
for (i in levels(kran$Category)){
  e <- e + geom_bar(data = kran[which(kran$Category == i), ],
                    mapping = aes(x = Category,
                                  fill = as.factor(Redeemed)),
                    width = 1,
                    color = "black") +
    ylab("Count") + xlab("Category")
}
e
#######################################################

#######################################

ggplot(data = kran,
       mapping = aes(x = factor(1),
                     fill = Theatre)) +
  geom_bar(width = 1) +
  coord_polar("y", start=0) +
  ylab(" ") + xlab(" ")

ggplot(data = kran,
       mapping = aes(x = factor(1),
                     fill = Producer)) +
  geom_bar(width = 1) +
  coord_polar("y", start=0) +
  ylab(" ") + xlab(" ")


ggplot(data = kran,
       mapping = aes(x = factor(1),
                     fill = Disposition)) +
  geom_bar(width = 1) +
  coord_polar("y", start=0)

ggplot(data = kran,
       mapping = aes(x = factor(1),
                     fill = PurchasedOnline)) +
  geom_bar(width = 1) +
  coord_polar("y", start=0)

ggplot(data = kran,
       mapping = aes(x = factor(1),
                     fill = Category)) +
  geom_bar(width = 1) +
  coord_polar("y", start=0)

##########################################################################


ggplot(data = kran[which(kran$Theatre == "Colwell Playhouse"), ],
       mapping = aes(x = EventCode)) +
  geom_histogram(stat = "count")

ggplot(data = kran[which(kran$Theatre == "FGH Stage, Salon Style"), ],
       mapping = aes(x = EventCode)) +
  geom_histogram(stat = "count")

ggplot(data = kran[which(kran$Theatre == "Foellinger Great Hall"), ],
       mapping = aes(x = EventCode)) +
  geom_histogram(stat = "count")

ggplot(data = kran[which(kran$Theatre == "Krannet Room"), ],
       mapping = aes(x = EventCode)) +
  geom_histogram(stat = "count")

ggplot(data = kran[which(kran$Theatre == "Studio Theatre"), ],
       mapping = aes(x = EventCode)) +
  geom_histogram(stat = "count")

ggplot(data = kran[which(kran$Theatre == "Tryon Festival Theatre"), ],
       mapping = aes(x = EventCode)) +
  geom_histogram(stat = "count")




############## Random Forest ############################

library(caret)
library(randomForest)

kran1 <- na.omit(kran)



RFmodel <- randomForest(x = kran1[, -which(names(kran) %in% c("Redeemed", "Seatblock", "PerformanceTitle",
                                                            "EventCode", "CustomerName", "Row", "TicketBarcode"))],
                        y = as.factor(kran1[, which(names(kran1) == "Redeemed")]),
                        ntree = 1000,
                        mtry = 1, 
                        nodesize = 25)


VariableImp <- varImp(RFmodel, scale = FALSE)

ggplot() +
  geom_bar(data = VariableImp,
           mapping = aes(x = reorder(row.names(VariableImp), Overall),
                         y = Overall),
           stat = "identity") +
  coord_flip() +
  xlab("Predictors") + 
  ylab("Variable Importance") +
  ggtitle("Feature Selection from Random Forest")

########### Cluster  (BAD) ##############################################

Cat <- kran1[, names(kran1) %in% c("PerformanceTitle", "EventCode", "Row")]
dist(Cat, method = "euclidean")

########## Logistic Regression #####################################

LRmodel <- glm(Redeemed ~.-Seatblock -PerformanceTitle- EventCode - +
                 CustomerName - Row - TicketBarcode,
               data = kran1,
               family = binomial(link = "logit"))

prediction_response <- predict(LRmodel, type = "response")
prediction <- rep(0, length = length(prediction_response))
for(i in 1:length(prediction)){
  prediction[i] <- ifelse(prediction_response[i] > 0.5, 1, 0)
}

table(kran1$Redeemed, prediction)

library(plotROC)
library(ROCR)
ROCRpred <- prediction(prediction_response, kran1$Redeemed)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")

ROCdata <- setNames(
  data.frame(ROCRperf@x.values, ROCRperf@y.values),
  nm = c("FPR", "TPR")
)

ROCdata <- data.frame(D = kran1$Redeemed,
                      D.str = c("No", "Yes")[kran1$Redeemed +1],
                      M = prediction_response,
                      stringsAsFactors = FALSE)

ggplot(data = ROCdata,
       mapping = aes(d = D,
                     m = M)) +
  geom_roc()



getSlots("performance")




