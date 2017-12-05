# **STAT 443** PM 2.5 Project
We have attempted three different algorithms here, Ordinary Least Square (OLS) Linear Regression, Random Forest and Stochastic Gradient Descent Linear Regression.

## 1. Data Preparation
### 1.1 Imputation (Missing Values)
The dataset contains many missing values due to different reasons. We would like to perform imputation to replace them with the substituded values. For continuous features, we replace the missing values with the median of the corresponding variables, which keeps their original central tendencies. Median is chosen here because of the skewness of our feature distributions. For categorical variables except `cbwd`, we treat missing values as a different level "NA". The missing values of variable `cbwd` is removed because of technical issues. 

### 1.2 Pre Variable Selection
Variables *No*, *year*, *day*, *season* and *Date* will not be taken into consideration. The reasons are listed below. 

**No**: This variable is used to help index each observation, which is similar to the primary key in the SQL query.  
**year**: This variable is one of the time indexes but do not have seasonal trend like *month* and *season*.  
**day**: This variable helps us record time, but does not contain seasonal trend, either.  
**season**: The information explained by this variable can be explained by variable *month* which contains more information.  
**Date**: Same as variable *No*. This variable is used to help index each observation, which is similar to the primary key in the SQL query.

We convert variable **Date** into a new variable **weekday** (Sunday, Monday, ...) based on the calendar. Hence, we will keep useful information that cannot be used directly in the dataset. 

## 3. Prediction
### 3.1 Ordinary Least Square Linear Regression
The parameters $\beta$ including the intercept is calculated by $\hat{\beta} = (X^T X)^{-1}X^Ty$. We would like to start from the full model with every features. Based on the dignostics plots, we notice that the model does not follow the OLS Regression assumptions and the dataset contains outliers. We perform variable log transformation to help solves these issues. The new diagnostics plots look much better than the previous four. 

Additionally, multicollinearity may also be a potential issue that influence the performance of our linear regression model. Variance Inflation Factor (VIF) is used here to help detect. Variables `DEWP` and `TEMP` are removed. 

### 3.2 Stochastic Gradient Descent Linear Regression
Stochastic Gradient Descent is another algorithm that helps us approach the weights ($\beta$s) of the linear regression. Unlike OLS, there is no mathematical assumption or restriction that prevents us from using it. We consider $loss = \displaystyle\frac{1}{n}\sum_{i=1}^{n}(y_i-\hat{y_i})^2$ as our Loss Function. We will try 1000 steps and the learning rate is eventually set to be 0.0000019 after many attempts. 

### 3.2 Random Forest
Every feature and response without transformation are considered here since Random Forest does not have to follow those OLS Regression assumptions. And Random Forest is one of the tree based models which are greedy learners. Hence, multicollinearity is not a problem, either. The tree number is set to be 100 because more trees will not change the performance basically but the running time will increase dramatically, and more trees may lead to overfitting. 

| ntree | train RMSE | error RMSE |
|:-----:|:----------:|:----------:|
|  100  |  12.63167  |  28.43827  |
|  500  |  12.46093  |  28.45565  |
