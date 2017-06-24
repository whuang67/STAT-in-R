set.seed(1000)
### Please install these packages if necessary
install.packages(c("dplyr", "ggplot2", "gridExtra", "lazyeval", "randomForest",
                   "sqldf", "pROC", "ipred", "e1071", "rpart", "rpart.plot"))

library(dplyr)
library(ggplot2)
library(gridExtra)
library(lazyeval)
library(randomForest)
library(sqldf)
library(pROC)
library(ipred)
library(e1071)
library(rpart)
library(rpart.plot)

### 1 Introduction #####################################################################################

### Read the dataset and assign variable names
Adult <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
                    sep=",",
                    header=FALSE,
                    col.names=c("age", "workclass", "fnlwgt", "education", "education_num",
                                "marital_status", "occupation", "relationship", "race", "sex", 
                                "capital_gain", "capital_loss", "hours_per_week",
                                "native_country", "salary"),
                    fill=FALSE,
                    strip.white=TRUE)
### Generate a numerical response
Adult$salary_num <- ifelse(Adult$salary == "<=50K", 0, 1)
### Ordinal variable "education"
Adult$education <- factor(Adult$education,
                          levels = c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th",
                                     "10th", "11th", "12th", "HS-grad", "Some-college",
                                     "Assoc-voc", "Assoc-acdm", "Bachelors", "Masters",
                                     "Prof-school", "Doctorate"))

### 1.1 Response variable and predicton metric #########################################################
### Frequency table and Pie plot
table_salary <- Adult %>% 
  group_by(salary) %>% 
  summarize(Freq = n()) %>% 
  mutate(Propotion = paste0(round(Freq/sum(Freq), 4)*100, "%"))
grid.arrange(
  tableGrob(table_salary, rows = NULL),
  ggplot(data = table_salary,
         mapping = aes(x = factor(1),
                       y = Freq,
                       fill = salary)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    geom_text(mapping = aes(label = Freq),
              position = position_stack(vjust = 0.5)) +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank()) +
    coord_polar(theta = "y") +
    ggtitle("Pie Chart of salary (Response)"),
  ncol = 2,
  widths = c(1/3, 2/3)
)

### 1.2 Duplicate variable #############################################################################
### Correlationship between "education" and "education_num"
grid.arrange(
  tableGrob(as.data.frame.matrix(table(Adult$education, Adult$education_num))),
  as.table = TRUE
)

#_______________________________________________________________________________________________________
### 2 Visualization and outlier detection ##############################################################
### 2.1 Categorical predictor variables ################################################################
### 2.1.1 Frequency bar plot ###########################################################################

### This is a function which can help us visualize the distribution of predictors
### For categorical variable, bar plot is drawn
### For discrete variable, histogram is drawn
### For continuous variable, density plot is drawn
Predictor_Plot <- function(Variable){
  dat <- Adult %>%
    group_by_(Variable) %>%
    summarize(Total = n())
  if(Variable %in% c("workclass", "education", "marital_status", "occupation",
                     "relationship", "race", "sex", "native_country")){
    
    
    plot <- ggplot(data = dat,
                   mapping = aes_string(x = Variable)) +
      geom_bar(mapping = aes(y = Total),
               stat = "identity") +
      theme(axis.text.x = element_text(angle = ifelse(Variable == "native_country",
                                                      -35, -20))) +
      ggtitle(paste0("Barplot of ", Variable)) +
      geom_text(mapping = aes(y = Total + 600,
                              label = Total),
                angle = -25)
    
  } else if(Variable %in% c("fnlwgt", "education_num", "capital_gain",
                            "capital_loss", "log(fnlwgt)", "log(capital_gain)",
                            "log(capital_loss)")){
    plot1 <- ggplot(data = Adult,
                    mapping = aes_string(y = Variable)) +
      geom_boxplot(mapping = aes(x = Variable)) +
      theme(axis.text.x = element_blank()) +
      xlab(Variable) +
      ggtitle(paste0("Boxplot of ", Variable))
    
    plot <- ggplot(data = Adult,
                   mapping = aes_string(x = Variable)) +
      geom_density(fill = "grey") +
      ggtitle(paste0("Density Plot of ", Variable))
    
  } else if(Variable %in% c("age", "hours_per_week",
                            "log(age)", "log(hours_per_week)")){
    plot <- ggplot(data = Adult,
                   mapping = aes_string(x = Variable)) +
      geom_histogram(color = "white",
                     stat = "count") +
      ggtitle(paste0("Histogram of ", Variable))
  }
  return(plot)
}

### Visualize categorical variables
grid.arrange(Predictor_Plot("education"),
             Predictor_Plot("workclass"),
             Predictor_Plot("marital_status"),
             ncol = 3,
             widths = c(1/3, 1/3, 1/3))
grid.arrange(Predictor_Plot("occupation"),
             Predictor_Plot("relationship"),
             Predictor_Plot("race"),
             ncol = 3,
             widths = c(1/3, 1/3, 1/3))
grid.arrange(Predictor_Plot("sex"),
             Predictor_Plot("native_country"),
             ncol = 2,
             widths = c(1/3, 2/3))

### 2.1.2 Percentage bar plot ##########################################################################
### This a function which can help us see correlationship between variable and response
### For categorical variables, percentage bar plot is drawn
### For numeircal (discrete or continuous) variables, box plot is drawn
Percentage_bar_plot <- function(Variable){
  if(Variable %in% c("workclass", "education", "marital_status", "occupation",
                     "relationship", "race", "sex", "native_country")){
    dat <- Adult %>%
      group_by_(Variable, "salary") %>%
      summarize(Total = n()) %>%
      group_by_(Variable) %>%
      mutate(Percentage = Total/sum(Total))
    
    plot <- ggplot(data = dat,
                   mapping = aes_string(x = Variable)) +
      geom_bar(mapping = aes(y = Percentage,
                             fill = salary),
               stat = "identity") +
      theme(axis.text.x = element_text(angle = ifelse(Variable == "native_country",
                                                      -35, -20))) +
      ggtitle(paste0("Percentage Barplot of ", Variable)) +
      geom_text(mapping = aes(y = Percentage,
                              label = paste0(round(Percentage, 4)*100, "%"),
                              fill = salary),
                position = position_stack(vjust = 0.5),
                angle = -90)
  } else if(Variable %in% c("capital_gain",
                            "capital_loss",
                            "age", "hours_per_week",
                            "fnlwgt", "log(fnlwgt)")){
    plot <- ggplot(data = Adult,
                   mapping = aes(x = salary)) +
      geom_boxplot(mapping = aes_string(y = Variable)) +
      ggtitle(paste0("Boxplot of ", Variable, " within salary"))
  }
  return(plot)
}

### Visualize the percentage bar plots (Response vs categorical variables)
g <- ggplotGrob(Percentage_bar_plot("workclass"))$grob
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
grid.arrange(Percentage_bar_plot("education") + theme(legend.position = "none"),
             Percentage_bar_plot("workclass") + theme(legend.position = "none"),
             Percentage_bar_plot("marital_status") + theme(legend.position = "none"),
             ncol = 3,
             widths = c(1/3, 1/3, 1/3))
grid.arrange(Percentage_bar_plot("occupation") + theme(legend.position = "none"),
             Percentage_bar_plot("relationship") + theme(legend.position = "none"),
             Percentage_bar_plot("race") + theme(legend.position = "none"),
             ncol = 3,
             widths = c(1/3, 1/3, 1/3))
grid.arrange(Percentage_bar_plot("sex") + theme(legend.position = "none"),
             Percentage_bar_plot("native_country") + theme(legend.position = "none"),
             legend,
             ncol = 3,
             widths = c(1/3, 7/12, 1/12))

### 2.2 Numerical variable #############################################################################
### 2.2.1 Discrete variable ############################################################################
### Visualize the discrete variables (by using function starting from line 68)
grid.arrange(Predictor_Plot("age"),
             Predictor_Plot("hours_per_week"),
             ncol = 2,
             widths = c(1/2, 1/2))

### 2.2.2 Continuous variable ##########################################################################
### Visualize the continuous variables (by using function starting from line 68)
grid.arrange(Predictor_Plot("capital_gain"),
             Predictor_Plot("capital_loss"),
             ncol = 2,
             widths = c(1/2, 1/2))
grid.arrange(Predictor_Plot("fnlwgt"),
             ncol = 2,
             widths = c(1/2, 1/2))

### 2.2.3 Box plot and outlier detection ###############################################################
### Function of draw a boxplot of one variable (not compared to the response)
### We build function to test outliers
Box_plot <- function(Variable){
  plot <- ggplot(data = Adult,
                 mapping = aes_string(x = factor(1),
                                      y = Variable)) +
    geom_boxplot() +
    theme(axis.text.x = element_blank()) +
    xlab(Variable) +
    ggtitle(paste0("Box plot of ", Variable))
  return(plot)
}

### Visualize to check potential outliers
### Visualize relationship between response and numerical variables
grid.arrange(Box_plot("age"),
             Percentage_bar_plot("age"),
             ncol = 2,
             widths = c(1/3, 2/3))
grid.arrange(Box_plot("hours_per_week"),
             Percentage_bar_plot("hours_per_week"),
             ncol = 2,
             widths = c(1/3, 2/3))
grid.arrange(Box_plot("capital_gain"),
             Percentage_bar_plot("capital_gain"),
             ncol = 2,
             widths = c(1/3, 2/3))
grid.arrange(Box_plot("capital_loss"),
             Percentage_bar_plot("capital_loss"),
             ncol = 2,
             widths = c(1/3, 2/3))
grid.arrange(Box_plot("fnlwgt"),
             Percentage_bar_plot("fnlwgt"),
             ncol = 2,
             widths = c(1/3, 2/3))

### 2.2.4 Correlation Coefficient Matrix ###############################################################

grid.arrange(
  tableGrob(
    round(cor(Adult[, names(Adult) %in% c("age", "hours_per_week", "capital_gain", "capital_loss",
                                          "fnlwgt")],
              use="pairwise.complete.obs"),
          4)))


#_______________________________________________________________________________________________________
### 3 Data cleaning, outliers and dataset spliting #####################################################
### 3.1 Missing values and data cleaning ###############################################################
### Part of the missing or invalid values
### "workclass", "occupation" also contain missing values, but they are not shown here
grid.arrange(
  tableGrob(
    Adult[which((Adult$sex == "Male" & Adult$relationship == "Wife") |
                  (Adult$sex == "Female" & Adult$relationship == "Husband") |
                  (Adult$workclass == "Never-worked" & Adult$occupation == "?")),
          names(Adult) %in% c("marital_status", "occupation", "relationship",
                              "sex", "workclass")]))

### Pairwise Deletion
Adult <- Adult[-which((Adult$sex == "Male" & Adult$relationship == "Wife") |
                        (Adult$sex == "Female" & Adult$relationship == "Husband")),]

### Treat "?" as one more category
Adult$occupation <- ifelse(Adult$workclass == "Never-worked" & Adult$occupation == "?",
                           "Unemployed",
                           ifelse(Adult$workclass == "?" & Adult$occupation == "?",
                                  "Other",
                                  as.character(Adult$occupation)))
Adult$workclass <- ifelse(Adult$workclass == "?", "Other", as.character(Adult$workclass))

### Tree-based imputation
Impute <- rpart::rpart(native_country ~ .,
                       data = Adult[which(Adult$native_country != "?"), ])

Adult$native_country1 <- predict(Impute, Adult, type = "class")
Adult$native_country <- ifelse(Adult$native_country == "?",
                               as.character(Adult$native_country1),
                               as.character(Adult$native_country))
Adult$native_country <- as.factor(Adult$native_country)
Adult$workclass <- as.factor(Adult$workclass)
Adult$occupation <- as.factor(Adult$occupation)
Adult$native_country1 <- NULL



### 3.2 Dataset spliting and outliers ##################################################################

### 80% Train, 20% Test
set.seed(1000)
Index <- sample(1:nrow(Adult), 6512)
Train_Adult <- Adult[-Index, ]
Test_Adult <- Adult[Index, ]

### Dataset (without outliers) spliting
### 80% Train, 20% Test
### Adult$age == 78 is Q3+1.5IQR
Adult1 <- Adult[which(Adult$age < 78), ]
set.seed(1000)
Index1 <- sample(1:nrow(Adult1), 6455)
Train_Adult1 <- Adult[-Index1, ]
Test_Adult1 <- Adult[Index1, ]

### 3.3 Feature selection ##############################################################################
### Random Forest model (Full set)
set.seed(1000)
fit <- randomForest(salary ~. - education_num  - salary_num, data = Train_Adult)
Var_Imp <- data.frame(importance(fit), Variable = row.names(importance(fit)), row.names = NULL)
Var_Imp1 <- sqldf("select Variable, MeanDecreaseGini from Var_Imp order by MeanDecreaseGini")
Var_Imp$Variable <- factor(Var_Imp$Variable,
                           levels = as.character(Var_Imp1$Variable))

ggplot(data = Var_Imp,
       mapping = aes(x = Variable)) +
  geom_bar(mapping = aes(y = MeanDecreaseGini),
           stat = "identity") + coord_flip() +
  geom_text(mapping = aes(y = MeanDecreaseGini + 100,
                          label = round(MeanDecreaseGini, 2))) +
  ggtitle("Variable Importance (Full dataset)")

### Random Forest model (Subset)
set.seed(1000)
fit1 <- randomForest(salary ~. - education_num  - salary_num, data = Train_Adult1)
Var_Imp <- data.frame(importance(fit1), Variable = row.names(importance(fit1)), row.names = NULL)
Var_Imp1 <- sqldf("select Variable, MeanDecreaseGini from Var_Imp order by MeanDecreaseGini")
Var_Imp$Variable <- factor(Var_Imp$Variable,
                           levels = as.character(Var_Imp1$Variable))

ggplot(data = Var_Imp,
       mapping = aes(x = Variable)) +
  geom_bar(mapping = aes(y = MeanDecreaseGini),
           stat = "identity") + coord_flip() +
  geom_text(mapping = aes(y = MeanDecreaseGini + 100,
                          label = round(MeanDecreaseGini, 2))) +
  ggtitle("Variable Importance (Subset)")

#_______________________________________________________________________________________________________
### 4 Model Fitting and Prediction ####################################################################
### 4.1 Logistic Regression ###########################################################################
### 4.1.1 Model with full dataset #####################################################################
### Model with all predictors (from feature selection)
LogisticRegression <- glm(salary_num ~ relationship + capital_gain + age + education + occupation +
                            marital_status + log(fnlwgt),
                          data = Train_Adult,
                          family = binomial(link = "logit"))
summary(LogisticRegression)
### ROC curve and AUC
a <- roc(response =  Train_Adult$salary_num,
         predictor = predict(LogisticRegression, Train_Adult, type = "response"))
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
LR_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (Logistic Reg) Alls")

b <- roc(response =  Test_Adult$salary_num,
         predictor = predict(LogisticRegression, Test_Adult, type = "response"))
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
LR_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Logistic Reg) All")
grid.arrange(LR_ROC, LR_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)


### Models without "education" because insignificance
LogisticRegression_a <- glm(salary_num ~ relationship + capital_gain + age + occupation +
                              marital_status + log(fnlwgt),
                            data = Train_Adult,
                            family = binomial(link = "logit"))
summary(LogisticRegression_a)

### ROC curve and AUC
a <- roc(response =  Train_Adult$salary_num,
         predictor = predict(LogisticRegression_a, Train_Adult, type = "response"))
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
LR_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (Logistic Reg2) All")

b <- roc(response =  Test_Adult$salary_num,
         predictor = predict(LogisticRegression_a, Test_Adult, type = "response"))
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
LR_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Logistic Reg2) All")
grid.arrange(LR_ROC, LR_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)

### 4.1.2 Logistic Regression Model with subset (Without outliers) #####################################
### Model with all predictors (from feature selection)
LogisticRegression1 <- glm(salary_num ~ relationship + capital_gain + age + education + occupation +
                             marital_status + fnlwgt,
                           data = Train_Adult1,
                           family = binomial(link = "logit"))
summary(LogisticRegression1)

### ROC curve and AUC
a <- roc(response =  Train_Adult1$salary_num,
         predictor = predict(LogisticRegression1, Train_Adult1, type = "response"))
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
LR_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (Logistic Reg) Subset")

b <- roc(response =  Test_Adult1$salary_num,
         predictor = predict(LogisticRegression1, Test_Adult1, type = "response"))
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
LR_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Logistic Reg) Subset")
grid.arrange(LR_ROC, LR_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)

### Model without "education" because of insignificance
LogisticRegression2 <- glm(salary_num ~ relationship + capital_gain + age + occupation +
                             marital_status + fnlwgt,
                           data = Train_Adult1,
                           family = binomial(link = "logit"))
summary(LogisticRegression2)

### ROC curve and AUC
a <- roc(response =  Train_Adult1$salary_num,
         predictor = predict(LogisticRegression2, Train_Adult1, type = "response"))
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
LR_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (Logistic Reg2) Subset")

b <- roc(response =  Test_Adult1$salary_num,
         predictor = predict(LogisticRegression2, Test_Adult1, type = "response"))
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
LR_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Logistic Reg2) Subset")
grid.arrange(LR_ROC, LR_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)

### 4.2 Classification and Regression Tree ############################################################
### 4.2.1 Model with full set##########################################################################
CART_fit <- rpart(salary ~ relationship + capital_gain + age + education + occupation +
                    marital_status + fnlwgt,
                  data = Train_Adult)
### Tree plot
rpart.plot(CART_fit, tweak = 1.2)

### ROC curve and AUC
a <- roc(response = Train_Adult$salary_num,
         predictor = predict(CART_fit, Train_Adult, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
CART_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (CART) All")

b <- roc(response = Test_Adult$salary_num,
         predictor = predict(CART_fit, Test_Adult, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
CART_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (CART) All")

grid.arrange(CART_ROC, CART_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)

### 4.2.2 Model with subset (Without outliers) ########################################################
CART_fit1 <- rpart(salary ~ relationship + capital_gain + age + education + occupation +
                     marital_status + fnlwgt,
                   data = Train_Adult1)
### Tree plot
rpart.plot(CART_fit1, tweak = 1.2)

### ROC curve and AUC
a <- roc(response = Train_Adult1$salary_num,
         predictor = predict(CART_fit1, Train_Adult1, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
CART_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (CART) Subset")

b <- roc(response = Test_Adult1$salary_num,
         predictor = predict(CART_fit1, Test_Adult1, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
CART_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (CART) Subset")

grid.arrange(CART_ROC, CART_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)

### 4.3 Naive Bayes ###################################################################################
### 4.3.1 Model with full set #########################################################################
Naive_Bayes_fit <- naiveBayes(salary ~ relationship + capital_gain + age + education + occupation +
                                marital_status + fnlwgt,
                              data = Train_Adult)
Naive_Bayes_fit

### ROC curve and AUC
a <- roc(response = Train_Adult$salary_num,
         predictor = predict(Naive_Bayes_fit, Train_Adult, type = "raw")[,2])
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
NaiveBayes_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (Naive Bayes) All")

b <- roc(response = Test_Adult$salary_num,
         predictor = predict(Naive_Bayes_fit, Test_Adult, type = "raw")[,2])
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
NaiveBayes_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Naive Bayes) All")

grid.arrange(NaiveBayes_ROC, NaiveBayes_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)


### 4.3.2 Model with subset ############################################################################
Naive_Bayes_fit1 <- naiveBayes(salary ~ relationship + capital_gain + age + education + occupation +
                                 marital_status + fnlwgt,
                               data = Train_Adult1)

Naive_Bayes_fit1

### ROC curve and AUC
a <- roc(response = Train_Adult1$salary_num,
         predictor = predict(Naive_Bayes_fit1, Train_Adult1, type = "raw")[,2])
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
NaiveBayes_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (Naive Bayes) Subset")

b <- roc(response = Test_Adult1$salary_num,
         predictor = predict(Naive_Bayes_fit1, Test_Adult1, type = "raw")[,2])
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
NaiveBayes_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Naive Bayes) Subset")

grid.arrange(NaiveBayes_ROC, NaiveBayes_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)

### 4.4 Bagging ########################################################################################
### 4.4.1 Model with full set ##########################################################################

### tune the parameter and find out the best one
Train_AUC_bag <- matrix(NA, nrow = 3, ncol = 1)
rownames(Train_AUC_bag) <- c("nbagg=15", "nbagg=20", "nbagg=25")
colnames(Train_AUC_bag) <- "Training AUC"
Test_AUC_bag <- matrix(NA, nrow = 3, ncol = 1)
rownames(Test_AUC_bag) <- c("nbagg=15", "nbagg=20", "nbagg=25")
colnames(Test_AUC_bag) <- "Testing AUC"
for(i in 1:3){
  set.seed(1000)
  Bagging_fit <- bagging(salary ~ relationship + capital_gain + age + education + occupation +
                           marital_status + fnlwgt,
                         data = Train_Adult,
                         nbagg = 5*i+10)
  a <- roc(response = Train_Adult$salary_num,
           predictor = predict(Bagging_fit, Train_Adult, type = "prob")[,2])
  Train_AUC_bag[i, 1] <- a$auc
  b <- roc(response = Test_Adult$salary_num,
           predictor = predict(Bagging_fit, Test_Adult, type = "prob")[,2])
  Test_AUC_bag[i, 1] <- b$auc
}

### List the result from parameter tuning
grid.arrange(
  tableGrob(round(Train_AUC_bag, 6)),
  tableGrob(round(Test_AUC_bag, 6)),
  ncol= 2, widths = c(1/2, 1/2)
)

### Use the best tuning parameter to fit model
set.seed(1000)
Bagging_fit <- bagging(salary ~ relationship + capital_gain + age + education + occupation +
                         marital_status + fnlwgt,
                       data = Train_Adult,
                       nbagg = 25)
Bagging_fit

### ROC curve and AUC
a <- roc(response = Train_Adult$salary_num,
         predictor = predict(Bagging_fit, Train_Adult, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
Bagging_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Traing ROC and AUC (Bagging) All")

b <- roc(response = Test_Adult$salary_num,
         predictor = predict(Bagging_fit, Test_Adult, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
Bagging_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Bagging) All")

grid.arrange(Bagging_ROC, Bagging_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)

### 4.4.2 Model with subset ############################################################################
### Tune the parameter and find out the best one
Train_AUC_bag1 <- matrix(NA, nrow = 3, ncol = 1)
rownames(Train_AUC_bag1) <- c("nbagg=15", "nbagg=20", "nbagg=25")
colnames(Train_AUC_bag1) <- "Training AUC"
Test_AUC_bag1 <- matrix(NA, nrow = 3, ncol = 1)
rownames(Test_AUC_bag1) <- c("nbagg=15", "nbagg=20", "nbagg=25")
colnames(Test_AUC_bag1) <- "Testing AUC"
for(i in 1:3){
  set.seed(1000)
  Bagging_fit1 <- bagging(salary ~ relationship + capital_gain + age + education + occupation +
                            marital_status + fnlwgt,
                          data = Train_Adult1,
                          nbagg = 5*i+10)
  a <- roc(response = Train_Adult1$salary_num,
           predictor = predict(Bagging_fit1, Train_Adult1, type = "prob")[,2])
  Train_AUC_bag1[i, 1] <- a$auc
  b <- roc(response = Test_Adult1$salary_num,
           predictor = predict(Bagging_fit1, Test_Adult1, type = "prob")[,2])
  Test_AUC_bag1[i, 1] <- b$auc
}


### List the results from parameter tuning
grid.arrange(
  tableGrob(round(Train_AUC_bag1, 6)),
  tableGrob(round(Test_AUC_bag1, 6)),
  ncol= 2, widths = c(1/2, 1/2)
)

### Use the best tuning parameter to fit model
set.seed(1000)
Bagging_fit1 <- bagging(salary ~ relationship + capital_gain + age + education + occupation +
                          marital_status + fnlwgt,
                        data = Train_Adult1,
                        nbagg = 25)

### ROC curve and AUC
a <- roc(response = Train_Adult1$salary_num,
         predictor = predict(Bagging_fit1, Train_Adult1, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
Bagging_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Traing ROC and AUC (Bagging) Subset")

b <- roc(response = Test_Adult1$salary_num,
         predictor = predict(Bagging_fit1, Test_Adult1, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
Bagging_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Bagging) Subset")

grid.arrange(Bagging_ROC, Bagging_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)

### 4.5 Random Forest ##################################################################################
### 4.5.1 Model with full set ##########################################################################
### Tune the parameter and find out the best ones
set.seed(1000)
Train_AUC <- matrix(NA, nrow = 3, ncol = 3)
colnames(Train_AUC) <- c("mtry=2", "mtry=3", "mtry=4")
rownames(Train_AUC) <- c("ntree=500", "ntree=1000", "ntree=1500")
Test_AUC <- matrix(NA, nrow = 3, ncol = 3)
colnames(Test_AUC) <- c("mtry=2", "mtry=3", "mtry=4")
rownames(Test_AUC) <- c("ntree=500", "ntree=1000", "ntree=1500")

for(i in 1:3){
  for(j in 1:3){
    set.seed(1000)
    RandomForest <- randomForest(salary ~ relationship + capital_gain + age + education + occupation +
                                   marital_status + fnlwgt,
                                 data = Train_Adult,
                                 ntree = 500*i,
                                 mtry = j +1)
    a <- roc(response = Train_Adult$salary_num,
             predictor = predict(RandomForest, Train_Adult, type = "prob")[,2])
    b <- roc(response = Test_Adult$salary_num,
             predictor = predict(RandomForest, Test_Adult, type = "prob")[,2])
    Train_AUC[i, j] <- a$auc
    Test_AUC[i, j] <- b$auc
    
  }
}


### List the results from parameter tuning
grid.arrange(
  tableGrob(round(Train_AUC, 6)),
  tableGrob(round(Test_AUC, 6)),
  ncol= 2, widths = c(1/2, 1/2)
)

### Use the best tuning parameters to fit model
set.seed(1000)
RandomForest <- randomForest(salary ~ relationship + capital_gain + age + education + occupation +
                               marital_status + fnlwgt,
                             data = Train_Adult,
                             ntree = 1000, mtry = 2)
RandomForest

### ROC curve and AUC
a <- roc(response = Train_Adult$salary_num,
         predictor = predict(RandomForest, Train_Adult, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
RF_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (Random Forest) All")

b <- roc(response = Test_Adult$salary_num,
         predictor = predict(RandomForest, Test_Adult, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
RF_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Random Forest) All")
grid.arrange(RF_ROC, RF_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)

### 4.5.2 Model with subset ############################################################################
### Tune the parameter and find out the best ones
set.seed(1000)
Train_AUC1 <- matrix(NA, nrow = 3, ncol = 3)
colnames(Train_AUC1) <- c("mtry=2", "mtry=3", "mtry=4")
rownames(Train_AUC1) <- c("ntree=500", "ntree=1000", "ntree=1500")
Test_AUC1 <- matrix(NA, nrow = 3, ncol = 3)
colnames(Test_AUC1) <- c("mtry=2", "mtry=3", "mtry=4")
rownames(Test_AUC1) <- c("ntree=500", "ntree=1000", "ntree=1500")

for(i in 1:3){
  for(j in 1:3){
    set.seed(1000)
    RandomForest1 <- randomForest(salary ~ relationship + capital_gain + age + education + occupation +
                                    marital_status + fnlwgt,
                                  data = Train_Adult1,
                                  ntree = 500*i,
                                  mtry = j +1)
    a <- roc(response = Train_Adult1$salary_num,
             predictor = predict(RandomForest1, Train_Adult1, type = "prob")[,2])
    b <- roc(response = Test_Adult1$salary_num,
             predictor = predict(RandomForest1, Test_Adult1, type = "prob")[,2])
    Train_AUC1[i, j] <- a$auc
    Test_AUC1[i, j] <- b$auc
    
  }
}


### List the results from parameter tuning
grid.arrange(
  tableGrob(round(Train_AUC1, 6)),
  tableGrob(round(Test_AUC1, 6)),
  ncol= 2, widths = c(1/2, 1/2)
)

### Use the best tuning parameters to fit model
set.seed(1000)
RandomForest1 <- randomForest(salary ~ relationship + capital_gain + age + education + occupation +
                                marital_status + fnlwgt,
                              data = Train_Adult1,
                              mtry = 3, ntree= 1500)
RandomForest1

### ROC curve and AUC
a <- roc(response = Train_Adult1$salary_num,
         predictor = predict(RandomForest1, Train_Adult1, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = a$sensitivities, Specificity = a$specificities)
RF_ROC <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(a$auc, 4)))) +
  ggtitle("Training ROC and AUC (Random Forest) Subset")

b <- roc(response = Test_Adult1$salary_num,
         predictor = predict(RandomForest1, Test_Adult1, type = "prob")[,2])
ROC_data <- data.frame(Sensitivity = b$sensitivities, Specificity = b$specificities)
RF_ROC_test <- ggplot(data = ROC_data) +
  geom_line(mapping = aes(x = Specificity,
                          y = Sensitivity)) +
  scale_x_continuous(trans = "reverse") +
  geom_abline(slope = 1, intercept = 1, color = "grey") +
  geom_text(mapping = aes(x = 0.25, y = 0.25, label = paste0("AUC = ", round(b$auc, 4)))) +
  ggtitle("Testing ROC and AUC (Random Forest) Subset")
grid.arrange(RF_ROC, RF_ROC_test,
             nrow = 1, widths = c(1/2, 1/2),
             ncol = 2)


AUC_fulldata <- data.frame(
  Model = c("Logistic Regression 1", "Logistic Regression 2",
            "CART", "Naive Bayes", "Bagging", "Random Forest"),
  Train_AUC = c(0.8981, 0.8831, 0.8452, 0.8708, 0.9999, 0.972),
  Test_AUC = c(0.8948, 0.8821, 0.8419, 0.8709, 0.8776, 0.8952)
)
AUC_fulldata$Model <- factor(AUC_fulldata$Model,
                             levels = c("Logistic Regression 1", "Logistic Regression 2",
                                        "CART", "Naive Bayes", "Bagging", "Random Forest"))
AUC_subset <- data.frame(
  Model = c("Logistic Regression 1", "Logistic Regression 2",
            "CART", "Naive Bayes", "Bagging", "Random Forest"),
  Train_AUC = c(0.8977, 0.8833, 0.8452, 0.8697, 0.9999, 0.9942),
  Test_AUC = c(0.8971, 0.8816, 0.8421, 0.8760, 0.8769, 0.8951)
)
AUC_subset$Model <- factor(AUC_subset$Model,
                           levels = c("Logistic Regression 1", "Logistic Regression 2",
                                      "CART", "Naive Bayes", "Bagging", "Random Forest"))

#_______________________________________________________________________________________________________
### 5 Comparison and conclusion ########################################################################
### 5.1 Comparison #####################################################################################
### AUCs vs different methods
summaryplot1 <- ggplot(data = AUC_fulldata,
                       mapping = aes(x = Model)) +
  geom_line(mapping = aes(y = Train_AUC, group = factor(1), color = "Train AUC")) +
  geom_point(mapping = aes(y = Train_AUC, color = "Train AUC")) +
  geom_line(mapping = aes(y = Test_AUC, group = factor(2), color = "Test AUC")) +
  geom_point(mapping = aes(y = Test_AUC, color = "Test AUC")) +
  ylab("Area under the Curve") + ggtitle("Models from full data") +
  theme(axis.text.x = element_text(angle = -15))

summaryplot2 <- ggplot(data = AUC_subset,
                       mapping = aes(x = Model)) +
  geom_line(mapping = aes(y = Train_AUC, group = factor(1), color = "Train AUC")) +
  geom_point(mapping = aes(y = Train_AUC, color = "Train AUC")) +
  geom_line(mapping = aes(y = Test_AUC, group = factor(2), color = "Test AUC")) +
  geom_point(mapping = aes(y = Test_AUC, color = "Test AUC")) +
  ylab("Area under the Curve") + ggtitle("Models from subset") +
  theme(axis.text.x = element_text(angle = -15))

grid.arrange(summaryplot1, summaryplot2,
             ncol = 2, widths = c(1/2, 1/2))
