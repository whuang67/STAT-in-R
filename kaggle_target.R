# library(readr)
dat <- read.csv("C:\\users\\Wenke\\downloads\\application_train.csv")
dat_test <- read.csv("C:\\users\\Wenke\\downloads\\application_test.csv")

# check percentage of NA ################################################
# both TRAIN and TEST contain NA values #################################
check_NA <- function(df){
  for(i in 1:122){
    temp = sum(is.na(df[, i]))
    if(temp>=1){
      print(paste0(names(df)[i], " -------  ", round(temp/nrow(df)*100, 2),
                   " ---  ", class(df[,i]),
                   " ---  ", median(df[,i], na.rm=TRUE)))
    }
  }
}
check_NA(dat)
check_NA(dat_test)

### merge train and test together to impute with the same distribution ###
dat_test$TARGET=2
wholething <- rbind(dat, dat_test)

# for(a in names(wholething)){
#   if(grepl("EXT_SOURCE_[0-9]", a)==TRUE){
#     print(a)
#   }
# }
# a <- names(wholething)[grepl("FLAG_DOCUMENT_[0-9]", names(wholething))]
# b <- apply(subset(wholething, select=a), 1, sum, na.rm=TRUE)
# wholething[7,]

for(i in 1:ncol(wholething)){
  if(class(wholething[,i])=="factor"){
    print(names(wholething)[i])
    print(length(table(wholething[,i])))
  }
}
table(wholething$EMERGENCYSTATE_MODE)
for(i in 1:ncol(wholething)){
  if(sum(is.na(wholething[,i]))/nrow(wholething)<0.2 && class(wholething[,i])=="numeric"){
    wholething[,i] <- ifelse(is.na(wholething[,i]), median(wholething[,i], na.rm=TRUE), wholething[,i])
  } else if(class(wholething[,i])=="factor"){
    wholething[,i] <- ifelse(trimws(wholething[,i], which="both")==""||is.na(wholething[,i]),
                             "Not_Available", wholething[,i])
  }
}

check_NA(wholething)
table(wholething$EXT_SOURCE_1)


library(ggplot2)
ggplot(data=wholething) + geom_histogram(mapping=aes(x=EXT_SOURCE_1))
ggplot(data=wholething) + geom_histogram(mapping=aes(x=EXT_SOURCE_2))
ggplot(data=wholething) + geom_histogram(mapping=aes(x=EXT_SOURCE_3))

# Print out the data type
for(i in 1:122){
  if(class(dat[,i])=="factor"){
    print(names(dat)[i])
    print(table(dat[,i]))
  }
}

for(i in 1:ncol(wholething)){
  temp=readline(prompt="press [enter] or [ANYTHING ELSE] to leave ")
  if(temp != "") break
  
  column = wholething[,i]
  var_name = names(wholething)[i]
  if(class(column)=="numeric"){
    plot = ggplot(data=wholething) + 
      geom_histogram(mapping=aes_string(x=var_name)) +
      ggtitle(paste("Histogram of", var_name))
    print(paste(var_name, "------------------ numeric!"))
    print(plot)
  } else if(class(column)=="factor"){
    print(paste(var_name, "------------------ factor!"))
  } else if(class(column)=="integer"){
    print(paste(var_name, "------------------ integer!"))
  }
}

class(wholething[,1])

# remove NA ############################################################
removeNA <- apply(wholething, 2, anyNA)
sum(removeNA) # see how many variables removed in total
wholething2 <- wholething[,!removeNA]
dat2 <- wholething2[wholething2$TARGET %in% c(0,1),]
dat_test2 <- wholething2[wholething2$TARGET==2,]; dat_test2$TARGET<-NULL

## calculate the proportion of missing values #########################
for(i in 1:122){
  temp=sum(is.na(dat[, i]))
  if(temp>=1){
    print(temp/307511)
  }
}

## Logistic Regression ####################################################
library(pROC)

bench_mark <- glm(TARGET ~.-SK_ID_CURR, data=dat2, family=binomial())
pred_benchmark <- predict(bench_mark, dat2, type="response")
auc(dat2$TARGET, pred_benchmark) # 77 variables: 0.7457
test_pred <- predict(bench_mark, dat_test2, type="response")

## CART ###################################################################
library(rpart)
cart_model <- rpart(TARGET~.-SK_ID_CURR, data=dat1, method="class")
# predict(cart_model, newdata=dat1, type="prob")
# mean(ifelse(predict(cart_model, newdata=dat1, type="prob")[,2]>=.5, 1, 0) == dat1$TARGET)
auc(dat1$TARGET, predict(cart_model, newdata=dat1, type="prob")[,2])


# Submission
sink("C:\\users\\Wenke\\downloads\\submission.csv")
cat("SK_ID_CURR,TARGET\n")
for(i in 1:nrow(dat_test)){
  cat(paste0(dat_test2$SK_ID_CURR[i], ",", test_pred[i], "\n"))
}
sink()
