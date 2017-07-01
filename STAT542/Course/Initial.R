library(readxl)
library(ggplot2)
library(stringr)
library(dplyr)
library(gglasso)

#Read the data
kran <- read_excel("~/R/STAT427/STATS 427%2c KCPA Ticket Redemption Data Fall 2016.xlsx", 
                   col_types = c("text", "numeric", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "numeric", "numeric", "text", 
                                 "text", "text", "numeric", "numeric", 
                                 "numeric", "text", "text", "numeric"))
#####processing#####
kran <- as.data.frame(kran)

#Split Seatblock
kran <- cbind(kran, t(do.call("cbind", strsplit(kran$Seatblock, ":"))))

names(kran) <- c("TicketBarcode", "CustomerNumber","CustomerName","EventCode",
                 "PerformanceTitle","Seatblock","PriceType","Redeemed","MailZip",
                 "Price","QuantityPurchased","Theatre","Disposition","PurchasedOnline",
                 "PurchaseDate","EventDate","DaysBetween","Producer","Category",
                 "Capacity","Level","Section","Row","Seat")
kran[,'Redeemed'] <- str_replace_all(kran[,'Redeemed']," ","")
kran[,'Redeemed'] <- ifelse(kran[,'Redeemed']=='Y',1,0)
kran[,"EventCode"] <- str_replace_all(kran[,"EventCode"]," ","")
kran[,"Producer"] <- str_replace_all(kran[,"Producer"],"illinois","Illinois")

#Create new variables QuantityPurchasedTotal and TicketSold 
kran <- kran %>% group_by(CustomerNumber) %>% mutate(QuantityPurchasedTotal = n())
kran <- kran %>% group_by(PerformanceTitle) %>% mutate(TicketSold = n())
kran <- as.data.frame(kran)

#Code NA
kran$MailZip <- ifelse(kran$MailZip=='0',NA,kran$MailZip)
kran$Disposition <- ifelse(kran$Disposition=='0',NA,kran$Disposition)
kran$DaysBetween <- ifelse(kran$DaysBetween<0|kran$DaysBetween>500,NA,kran$DaysBetween)
kran$Capacity <- ifelse(kran$Capacity==0,NA,kran$Capacity)

#Transform MailZip
str <- c('61820','61821','61822','61824','61825','61826','61801','61802','61803','MC','61874')
# data$Zip <- any(str_detect(data$MailZip,str))
kran$MailZip <- as.numeric(ifelse(is.na(kran$MailZip),NA,apply(sapply(str, grepl, kran$MailZip), 1, any)))
write.csv(kran,file = '~/R/Stat427/Cleaned_Ticket Redemption Data.csv',row.names=FALSE)

data <- as.data.frame(kran[,c("PriceType","MailZip","Theatre","Disposition","PurchasedOnline",
                              "Producer","Category","Level","Section",
                              "DaysBetween","QuantityPurchased","Price","Capacity",
                              'QuantityPurchasedTotal','TicketSold',"Redeemed")])

for (i in c("PriceType","MailZip","Theatre","Disposition",
            "PurchasedOnline","Producer","Category","Level","Section")){
  data[,i] = as.factor(data[,i])
}
# data[,'Seat'] <- as.numeric(data[,'Seat'])
levels(data[,'PriceType']) <- c('C','P',rep('SA',4),rep('SC',4),'SP',rep('SU',4),rep('UI',4),rep('YT',4))
data$Section <- ifelse(data$Section=='C','C','NC')
data$Level <- ifelse(data$Level=='B','B','M')

#####screen#####
p.val.lm = rep(0,dim(data)[2]-1)
for (i in 1:15){
  f = summary(lm(Redeemed ~. ,data=data[,c(i,dim(data)[2])]))$fstatistic
  p.val.lm[i] = pf(f[1],f[2],f[3], lower.tail = F)
}
p.val.glm = rep(0,dim(data)[2]-1)
for (i in 1:15){
  model = glm(Redeemed ~. ,family=binomial,data=data[,c(i,dim(data)[2])])
  p.val.glm[i] = pchisq(model$null.deviance-model$deviance, model$df.null-model$df.residual, lower.tail = F)
}
#So all the variables except 'Section' are marginally significant
data$Section <- NULL

###### Group lasso using gglasso ######
full.model <- lm(Redeemed~.,data=data)
model.mat <- model.matrix(full.model)
mygg <- cv.gglasso(model.mat, data$Redeemed[rowSums(!is.na(data))==dim(data)[2]],
                   group = c(1,rep(2,6),3,rep(4,5),rep(5,5),6,rep(7,3),rep(8,9),9:15))
coef(mygg,s='lambda.1se')
#So 'Zip', 'Disposition', 'Producer', and 'Capacity' are penalized to 0
data[,c('Zip', 'Disposition', 'Producer', 'Capacity')] <- NULL

#####logistic model#####
fullmod <- glm(Redeemed ~. ,family=binomial,data=data)
summary(fullmod)
y.pred <- predict(fullmod,newdata=data,type='response')
y.pred <- ifelse(y.pred>0.5,1,0)
table(y.pred,data[,'Redeemed'])
#should use cv error