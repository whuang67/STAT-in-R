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
The parameters $\beta$ including the intercept is calculated by $\hat{\beta} = (X^T X)^{-1}X^Ty$. 

### 3.2 Random Forest

### 3.3 Stochastic Gradient Descent Linear Regression
