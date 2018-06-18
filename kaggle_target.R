# library(readr)
library(ggplot2)
dat <- read.csv("C:\\users\\Wenke\\downloads\\application_train.csv")
dat_test <- read.csv("C:\\users\\Wenke\\downloads\\application_test.csv")
# table(dat_test$NAME_FAMILY_STATUS)
# table(wholething2$NAME_FAMILY_STATUS)
# dat <- read.csv("C:\\users\\whuang67\\downloads\\kaggle\\application_train.csv")
# dat_test <- read.csv("C:\\users\\whuang67\\downloads\\kaggle\\application_test.csv")
idx <- sample(1:307511, 307511-48744)

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
  if(sum(is.na(wholething[,i]))/nrow(wholething)<0.2 && class(wholething[,i])=="numeric"){
    wholething[,i] <- ifelse(is.na(wholething[,i]),
                             median(wholething[,i], na.rm=TRUE),
                             wholething[,i])
  } else if(names(wholething)[i]=="NAME_FAMILY_STATUS"){
    wholething[,i] <- factor(ifelse(wholething[,i]=="Unknown",
                                    "Married",
                                    as.character(wholething[,i])))
  }
  # else if(class(wholething[,i])=="factor"){
  #   wholething[,i] <- as.character(wholething[,i])
  #   wholething[,i] <- ifelse(trimws(wholething[,i], which="both")==""||is.na(wholething[,i]),
  #                            "Not_Available", wholething[,i])
  #   wholething[,i] <- as.factor(wholething[,i])
  # }
}

check_NA(wholething)
table(wholething$EXT_SOURCE_1)

factor(ifelse(wholething$NAME_FAMILY_STATUS=="Unknown", "Married", as.character(wholething$NAME_FAMILY_STATUS))) -> wholething$NAME_FAMILY_STATUS
# Print out the data type

for(i in 1:ncol(wholething2)){
  temp=readline(prompt="press [enter] or [ANYTHING ELSE] to leave ")
  if(temp != "") break
  
  column = wholething2[,i]
  var_name = names(wholething2)[i]
  if(class(column)=="numeric"){
    plot = ggplot(data=wholething2) +
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

ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(AMT_INCOME_TOTAL)))
ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(AMT_CREDIT)))
ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(AMT_ANNUITY))) ## missing
ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(AMT_GOODS_PRICE))) ## missing
ggplot(data=wholething) + geom_histogram(mapping=aes(x=REGION_POPULATION_RELATIVE))
ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(100-DAYS_REGISTRATION)))
# ggplot(data=wholething) + geom_histogram(mapping=aes(x=OWN_CAR_AGE)) ## missing
ggplot(data=wholething) + geom_histogram(mapping=aes(x=REGION_POPULATION_RELATIVE))
ggplot(data=wholething) + geom_histogram(mapping=aes(x=CNT_FAM_MEMBERS))
# ggplot(data=wholething) + geom_histogram(mapping=aes(x=EXT_SOURCE_1)) ## missing
ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(1-EXT_SOURCE_2))) ## missing
ggplot(data=wholething) + geom_histogram(mapping=aes(x=EXT_SOURCE_3)) ## missing
# ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(1+APARTMENTS_AVG))) ## missing
# ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(1+BASEMENTAREA_AVG))) ## missing
ggplot(data=wholething) + 
  geom_histogram(mapping=aes(x=YEARS_BEGINEXPLUATATION_AVG)) +
  xlim(0.93, 1.05) ## missing???
ggplot(data=wholething) + geom_histogram(mapping=aes(x=YEARS_BUILD_AVG)) ## missing
ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(1+COMMONAREA_AVG))) ## missing??
ggplot(data=wholething) + geom_histogram(mapping=aes(x=ELEVATORS_AVG)) ## missing
ggplot(data=wholething) + geom_histogram(mapping=aes(x=log(1+ENTRANCES_AVG)))

ggplot(data=wholething2) + geom_histogram(mapping=aes(x=FLAG_MOBIL)) + xlim(0.99, 1.05) ## removed only 2 levels
ggplot(data=wholething2) + geom_histogram(mapping=aes(x=REGION_RATING_CLIENT)) ## 3 levels, catigorical would be better
table(wholething2$REGION_RATING_CLIENT)


min(wholething$AMT_GOODS_PRICE, na.rm=TRUE)
# remove NA ############################################################
removeNA <- apply(wholething, 2, anyNA)
sum(removeNA) # see how many variables removed in total
wholething2 <- wholething[,!removeNA]
dat2 <- wholething2[wholething2$TARGET %in% c(0,1),]
dat_test2 <- wholething2[wholething2$TARGET==2,]; dat_test2$TARGET<-NULL

noquote(names(dat2))
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

## Naive Bayes ############################################################
library(e1071)
nb_1st <- naiveBayes(TARGET ~ log(1-EXT_SOURCE_2) +
                       EXT_SOURCE_3 +
                       DAYS_BIRTH +
                       DAYS_ID_PUBLISH +
                       log(100-DAYS_REGISTRATION) +
                       log(AMT_ANNUITY) +
                       DAYS_EMPLOYED +
                       DAYS_LAST_PHONE_CHANGE +
                       log(AMT_CREDIT) +
                       OCCUPATION_TYPE +
                       log(AMT_INCOME_TOTAL) +
                       REGION_POPULATION_RELATIVE +
                       WEEKDAY_APPR_PROCESS_START +
                       log(AMT_GOODS_PRICE) +
                       HOUR_APPR_PROCESS_START +
                       AMT_REQ_CREDIT_BUREAU_YEAR +
                       OBS_30_CNT_SOCIAL_CIRCLE +
                       OBS_60_CNT_SOCIAL_CIRCLE +
                       NAME_FAMILY_STATUS +
                       CNT_FAM_MEMBERS +
                       WALLSMATERIAL_MODE,
                     data = dat2[idx,])
pred_nb_1 <- predict(nb_1st, newdata=dat2[-idx,], type="raw")
auc(dat2$TARGET[-idx], pred_nb_1[,2])
auc(dat2$TARGET[idx], predict(nb_1st, newdata=dat2[idx,], type="raw")[,2])
## LOGISTIC REGRESSION, seriously #########################################
lr <- glm(TARGET ~ NAME_CONTRACT_TYPE +
            CODE_GENDER +
            FLAG_OWN_CAR +
            FLAG_OWN_REALTY +
            CNT_CHILDREN +
            log(AMT_INCOME_TOTAL) +
            log(AMT_CREDIT) +
            log(AMT_ANNUITY) + 
            log(AMT_GOODS_PRICE) +
            NAME_TYPE_SUITE +
            NAME_INCOME_TYPE + 
            NAME_EDUCATION_TYPE +
            NAME_FAMILY_STATUS +
            NAME_HOUSING_TYPE +
            REGION_POPULATION_RELATIVE +
            DAYS_BIRTH +
            DAYS_EMPLOYED +
            log(100-DAYS_REGISTRATION) +
            DAYS_ID_PUBLISH +
            FLAG_MOBIL +
            FLAG_EMP_PHONE +
            FLAG_WORK_PHONE +
            FLAG_CONT_MOBILE +
            FLAG_PHONE +
            FLAG_EMAIL +
            OCCUPATION_TYPE +
            CNT_FAM_MEMBERS +
            REGION_RATING_CLIENT +
            REGION_RATING_CLIENT_W_CITY +
            WEEKDAY_APPR_PROCESS_START +
            HOUR_APPR_PROCESS_START +
            REG_REGION_NOT_LIVE_REGION +
            REG_REGION_NOT_WORK_REGION +
            LIVE_REGION_NOT_WORK_REGION +
            REG_CITY_NOT_LIVE_CITY +
            REG_CITY_NOT_WORK_CITY +
            LIVE_CITY_NOT_WORK_CITY +
            ORGANIZATION_TYPE +
            log(1-EXT_SOURCE_2) +
            EXT_SOURCE_3 +
            FONDKAPREMONT_MODE +
            HOUSETYPE_MODE +
            WALLSMATERIAL_MODE +
            EMERGENCYSTATE_MODE +
            OBS_30_CNT_SOCIAL_CIRCLE +
            DEF_30_CNT_SOCIAL_CIRCLE +
            OBS_60_CNT_SOCIAL_CIRCLE +
            DEF_60_CNT_SOCIAL_CIRCLE +
            DAYS_LAST_PHONE_CHANGE +
            FLAG_DOCUMENT_2 +
            FLAG_DOCUMENT_3 +
            FLAG_DOCUMENT_4 + 
            FLAG_DOCUMENT_5 +
            FLAG_DOCUMENT_6 +
            FLAG_DOCUMENT_7 +
            FLAG_DOCUMENT_8 +
            FLAG_DOCUMENT_9 +
            FLAG_DOCUMENT_10 +
            FLAG_DOCUMENT_11 +
            FLAG_DOCUMENT_12 +
            FLAG_DOCUMENT_13 + 
            FLAG_DOCUMENT_14 +
            FLAG_DOCUMENT_15 +
            FLAG_DOCUMENT_16 + 
            FLAG_DOCUMENT_17 +
            FLAG_DOCUMENT_18 +
            FLAG_DOCUMENT_19 +
            FLAG_DOCUMENT_20 +
            FLAG_DOCUMENT_21 +
            AMT_REQ_CREDIT_BUREAU_HOUR +
            AMT_REQ_CREDIT_BUREAU_DAY +
            AMT_REQ_CREDIT_BUREAU_WEEK + 
            AMT_REQ_CREDIT_BUREAU_MON +
            AMT_REQ_CREDIT_BUREAU_QRT +
            AMT_REQ_CREDIT_BUREAU_YEAR,
          data = dat2,
          family = binomial())
pred_lr <- predict(lr, newdata=dat2, type="response")
auc(dat2$TARGET, pred_lr)
test_pred <- predict(lr, newdata=dat_test2, type="response")


lr_2 <- glm(TARGET ~ log(1-EXT_SOURCE_2) +
              EXT_SOURCE_3 +
              DAYS_BIRTH +
              DAYS_ID_PUBLISH +
              log(100-DAYS_REGISTRATION) +
              log(AMT_ANNUITY) +
              DAYS_EMPLOYED +
              DAYS_LAST_PHONE_CHANGE +
              log(AMT_CREDIT) +
              OCCUPATION_TYPE +
              log(AMT_INCOME_TOTAL) +
              REGION_POPULATION_RELATIVE +
              WEEKDAY_APPR_PROCESS_START +
              log(AMT_GOODS_PRICE) +
              HOUR_APPR_PROCESS_START +
              AMT_REQ_CREDIT_BUREAU_YEAR,
            data = dat2,
            family = binomial())
pred_lr2 <- predict(lr_2, newdata=dat2, type="response")
auc(dat2$TARGET, pred_lr2)


lr_3 <- glm(TARGET ~ log(1-EXT_SOURCE_2) +
              EXT_SOURCE_3 +
              DAYS_BIRTH +
              DAYS_ID_PUBLISH +
              log(100-DAYS_REGISTRATION) +
              log(AMT_ANNUITY) +
              DAYS_EMPLOYED +
              DAYS_LAST_PHONE_CHANGE +
              log(AMT_CREDIT) +
              OCCUPATION_TYPE +
              log(AMT_INCOME_TOTAL) +
              REGION_POPULATION_RELATIVE +
              WEEKDAY_APPR_PROCESS_START +
              log(AMT_GOODS_PRICE) +
              HOUR_APPR_PROCESS_START +
              AMT_REQ_CREDIT_BUREAU_YEAR +
              OBS_30_CNT_SOCIAL_CIRCLE +
              OBS_60_CNT_SOCIAL_CIRCLE +
              NAME_FAMILY_STATUS +
              CNT_FAM_MEMBERS +
              WALLSMATERIAL_MODE,
            data = dat2[idx,],
            family = binomial())
pred_lr3 <- predict(lr_3, newdata=dat2[-idx,], type="response")
auc(dat2$TARGET[-idx], pred_lr3)
# cor(pred_lr3, pred_nb_1[,2], method="pearson")

test_pred <- predict(lr_3, newdata=dat_test2, type="response") 
            # NAME_CONTRACT_TYPE +
            # CODE_GENDER +
            # FLAG_OWN_CAR +
            # FLAG_OWN_REALTY +
            # CNT_CHILDREN +
            # 
            # NAME_TYPE_SUITE +
            # NAME_INCOME_TYPE + 
            # NAME_EDUCATION_TYPE +
            # 
            # NAME_HOUSING_TYPE +
            # 
            # FLAG_MOBIL +
            # FLAG_EMP_PHONE +
            # FLAG_WORK_PHONE +
            # FLAG_CONT_MOBILE +
            # FLAG_PHONE +
            # FLAG_EMAIL +
            # 
            # 
            # REGION_RATING_CLIENT +
            # REGION_RATING_CLIENT_W_CITY +
            # 
            # REG_REGION_NOT_LIVE_REGION +
            # REG_REGION_NOT_WORK_REGION +
            # LIVE_REGION_NOT_WORK_REGION +
            # REG_CITY_NOT_LIVE_CITY +
            # REG_CITY_NOT_WORK_CITY +
            # LIVE_CITY_NOT_WORK_CITY +
            # ORGANIZATION_TYPE +
            # 
            # FONDKAPREMONT_MODE +
            # HOUSETYPE_MODE +
            # 
            # EMERGENCYSTATE_MODE +
            
            # DEF_30_CNT_SOCIAL_CIRCLE +
            # OBS_60_CNT_SOCIAL_CIRCLE +
            # DEF_60_CNT_SOCIAL_CIRCLE +
            # 
            # FLAG_DOCUMENT_2 +
            # FLAG_DOCUMENT_3 +
            # FLAG_DOCUMENT_4 + 
            # FLAG_DOCUMENT_5 +
            # FLAG_DOCUMENT_6 +
            # FLAG_DOCUMENT_7 +
            # FLAG_DOCUMENT_8 +
            # FLAG_DOCUMENT_9 +
            # FLAG_DOCUMENT_10 +
            # FLAG_DOCUMENT_11 +
            # FLAG_DOCUMENT_12 +
            # FLAG_DOCUMENT_13 + 
            # FLAG_DOCUMENT_14 +
            # FLAG_DOCUMENT_15 +
            # FLAG_DOCUMENT_16 + 
            # FLAG_DOCUMENT_17 +
            # FLAG_DOCUMENT_18 +
            # FLAG_DOCUMENT_19 +
            # FLAG_DOCUMENT_20 +
            # FLAG_DOCUMENT_21 +
            # AMT_REQ_CREDIT_BUREAU_HOUR +
            # AMT_REQ_CREDIT_BUREAU_DAY +
            # AMT_REQ_CREDIT_BUREAU_WEEK + 
            # AMT_REQ_CREDIT_BUREAU_MON +
            # AMT_REQ_CREDIT_BUREAU_QRT +

## LASSO ##################################################################
library(Matrix)
library(glmnet)
wholething3 <- sparse.model.matrix(TARGET ~ .-SK_ID_CURR-1, data=wholething2)
dat3 <- wholething3[1:307511, ]
dat_test3 <- wholething3[307512:356255, ]
predictor <- dat$TARGET
lasso1 <- cv.glmnet(x=as.matrix(dat3), y=as.matrix(predictor), nfolds=5, family="binomial", type.measure="auc")
pred_lasso1 <- predict(lasso1, dat3, type="response")

## CART ###################################################################
library(rpart)
cart_model <- rpart(TARGET~.-SK_ID_CURR, data=dat1, method="class")
# predict(cart_model, newdata=dat1, type="prob")
# mean(ifelse(predict(cart_model, newdata=dat1, type="prob")[,2]>=.5, 1, 0) == dat1$TARGET)
auc(dat1$TARGET, predict(cart_model, newdata=dat1, type="prob")[,2])

## Random Forest #########################################################
library(randomForest)

# dat <- NULL; dat_test<-NULL; dat2<- NULL; dat_test2<- NULL; wholething<-NULL; wholething2<- NULL
# rf_model_1st <- randomForest(x=as.matrix(dat3), y=as.factor(dat$TARGET), ntree=500, importance=TRUE)
for(i in 3:122){
  if(class(dat2[,i])=="factor"){
    print(names(dat2)[i])
    print(length(table(dat2[,i])))
  }
}
rf_model_1st <- randomForest(as.factor(TARGET) ~ NAME_CONTRACT_TYPE +
                               CODE_GENDER +
                               FLAG_OWN_CAR +
                               FLAG_OWN_REALTY +
                               CNT_CHILDREN +
                               AMT_INCOME_TOTAL +
                               AMT_CREDIT +
                               AMT_ANNUITY + 
                               AMT_GOODS_PRICE +
                               NAME_TYPE_SUITE +
                               NAME_INCOME_TYPE + 
                               NAME_EDUCATION_TYPE +
                               NAME_FAMILY_STATUS +
                               NAME_HOUSING_TYPE +
                               REGION_POPULATION_RELATIVE +
                               DAYS_BIRTH +
                               DAYS_EMPLOYED +
                               DAYS_REGISTRATION +
                               DAYS_ID_PUBLISH +
                               FLAG_MOBIL +
                               FLAG_EMP_PHONE +
                               FLAG_WORK_PHONE +
                               FLAG_CONT_MOBILE +
                               FLAG_PHONE +
                               FLAG_EMAIL +
                               OCCUPATION_TYPE +
                               CNT_FAM_MEMBERS +
                               REGION_RATING_CLIENT +
                               REGION_RATING_CLIENT_W_CITY +
                               WEEKDAY_APPR_PROCESS_START +
                               HOUR_APPR_PROCESS_START +
                               REG_REGION_NOT_LIVE_REGION +
                               REG_REGION_NOT_WORK_REGION +
                               LIVE_REGION_NOT_WORK_REGION +
                               REG_CITY_NOT_LIVE_CITY +
                               REG_CITY_NOT_WORK_CITY +
                               LIVE_CITY_NOT_WORK_CITY +
                               # ORGANIZATION_TYPE +
                               EXT_SOURCE_2 +
                               EXT_SOURCE_3 +
                               FONDKAPREMONT_MODE +
                               HOUSETYPE_MODE +
                               WALLSMATERIAL_MODE +
                               EMERGENCYSTATE_MODE +
                               OBS_30_CNT_SOCIAL_CIRCLE +
                               DEF_30_CNT_SOCIAL_CIRCLE +
                               OBS_60_CNT_SOCIAL_CIRCLE +
                               DEF_60_CNT_SOCIAL_CIRCLE +
                               DAYS_LAST_PHONE_CHANGE +
                               FLAG_DOCUMENT_2 +
                               FLAG_DOCUMENT_3 +
                               FLAG_DOCUMENT_4 + 
                               FLAG_DOCUMENT_5 +
                               FLAG_DOCUMENT_6 +
                               FLAG_DOCUMENT_7 +
                               FLAG_DOCUMENT_8 +
                               FLAG_DOCUMENT_9 +
                               FLAG_DOCUMENT_10 +
                               FLAG_DOCUMENT_11 +
                               FLAG_DOCUMENT_12 +
                               FLAG_DOCUMENT_13 + 
                               FLAG_DOCUMENT_14 +
                               FLAG_DOCUMENT_15 +
                               FLAG_DOCUMENT_16 + 
                               FLAG_DOCUMENT_17 +
                               FLAG_DOCUMENT_18 +
                               FLAG_DOCUMENT_19 +
                               FLAG_DOCUMENT_20 +
                               FLAG_DOCUMENT_21 +
                               AMT_REQ_CREDIT_BUREAU_HOUR +
                               AMT_REQ_CREDIT_BUREAU_DAY +
                               AMT_REQ_CREDIT_BUREAU_WEEK + 
                               AMT_REQ_CREDIT_BUREAU_MON +
                               AMT_REQ_CREDIT_BUREAU_QRT +
                               AMT_REQ_CREDIT_BUREAU_YEAR,
                             data = dat2,
                             importance=TRUE,
                             ntree=50)
# print(rf_model_1st$importance)
imp <- importance(rf_model_1st, type=2)
imp_idx <- sort.int(imp, index.return=TRUE)$ix
ggplot() + 
  geom_bar(mapping=aes(x=row.names(imp),
                       y=imp),
           stat="identity") + 
  coord_flip() +
  scale_x_discrete(limits=row.names(imp)[imp_idx]) +
  geom_hline(yintercept=160,#quantile(imp, probs=0.25),
             color="red")
cat(paste(row.names(imp)[which(imp>=160)], collapse="+\n"))


rf_model_2nd <- randomForest(as.factor(TARGET) ~ EXT_SOURCE_2 +
                               EXT_SOURCE_3 +
                               DAYS_BIRTH +
                               DAYS_ID_PUBLISH +
                               DAYS_REGISTRATION +
                               AMT_ANNUITY +
                               DAYS_EMPLOYED +
                               DAYS_LAST_PHONE_CHANGE +
                               AMT_CREDIT +
                               OCCUPATION_TYPE +
                               AMT_INCOME_TOTAL +
                               REGION_POPULATION_RELATIVE +
                               WEEKDAY_APPR_PROCESS_START +
                               AMT_GOODS_PRICE +
                               HOUR_APPR_PROCESS_START +
                               AMT_REQ_CREDIT_BUREAU_YEAR +
                               OBS_30_CNT_SOCIAL_CIRCLE +
                               OBS_60_CNT_SOCIAL_CIRCLE +
                               NAME_FAMILY_STATUS +
                               CNT_FAM_MEMBERS +
                               WALLSMATERIAL_MODE,
                             data=dat2[idx,],
                             ntree=200)

rf_pred_2nd <- predict(rf_model_2nd, dat2[-idx,], type="prob")
print(auc(dat2$TARGET[-idx], rf_pred_2nd[,2]))

library(caret)
control <- trainControl(method="cv", number=10, verboseIter=TRUE)
tunegrid <- expand.grid(mtry=c(4,5,6,7))
rf_caret <- train(as.factor(TARGET) ~ EXT_SOURCE_2 +
                    EXT_SOURCE_3 +
                    DAYS_BIRTH +
                    DAYS_ID_PUBLISH +
                    DAYS_REGISTRATION +
                    AMT_ANNUITY +
                    DAYS_EMPLOYED +
                    DAYS_LAST_PHONE_CHANGE +
                    AMT_CREDIT +
                    OCCUPATION_TYPE +
                    AMT_INCOME_TOTAL +
                    REGION_POPULATION_RELATIVE +
                    WEEKDAY_APPR_PROCESS_START +
                    AMT_GOODS_PRICE +
                    HOUR_APPR_PROCESS_START +
                    AMT_REQ_CREDIT_BUREAU_YEAR +
                    OBS_30_CNT_SOCIAL_CIRCLE +
                    OBS_60_CNT_SOCIAL_CIRCLE +
                    NAME_FAMILY_STATUS +
                    CNT_FAM_MEMBERS +
                    WALLSMATERIAL_MODE,
                  data=dat2[idx,],
                  method="rf",
                  trControl=control,
                  metric="Accuracy",
                  tuneGrid=tunegrid,
                  ntree=200,
                  preProcess=c("center", "scale"),
                  verbose=TRUE)
print(rf_caret)
# library(pROC)

test_pred <- predict(rf_model_2nd, newdata=dat_test2, type="prob")[,2]
## xgboost ###############################################################
library(xgboost)

# dim(wholething3)
# class(dat3)

xgb_model <- xgboost(data = dat3[idx,],
                     label = predictor[idx],
                     nrounds=300,
                     eta=0.02,
                     objective = "binary:logistic")
xgb_pred <- predict(xgb_model, dat3[-idx,])
auc(dat2$TARGET[-idx], xgb_pred)
plot(roc(dat2$TARGET[-idx], xgb_pred))
test_pred <- predict(xgb_model, dat_test3)

# Submission
sink("C:\\users\\whuang67\\downloads\\kaggle\\submission.csv")
cat("SK_ID_CURR,TARGET\n")
for(i in 1:nrow(dat_test)){
  cat(paste0(dat_test2$SK_ID_CURR[i], ",", test_pred[i], "\n"))
}
sink()
