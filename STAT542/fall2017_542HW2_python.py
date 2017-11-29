# -*- coding: utf-8 -*-
"""
Created on Thu Nov 23 11:38:54 2017

@author: whuang67
"""

############################################
### Question 1 #############################
############################################
import os
os.chdir("C:/users/whuang67/downloads")
import numpy as np
import pandas as pd

dat = pd.read_csv("bitcoin_dataset.csv")
del dat["btc_trade_volume"]; del dat["Date"]
dat_train = dat[0:1460]
dat_test = dat[1460:1588]

import itertools
names_list = dat.columns.tolist()
names_list.remove("btc_market_price")
y = dat_train["btc_market_price"]
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
best_model = {"error": None, "predictors": None}

def Linear_Regression(names):
    X = np.array(dat_train[names])
    y = np.array(dat_train["btc_market_price"])
    X_test = np.array(dat_test[names])
    y_test = np.array(dat_test["btc_market_price"])
    reg = LinearRegression().fit(X, y)
    return mean_squared_error(y_test, reg.predict(X_test))

variable_names = [list(names) for i in range(1, len(names_list)+1) \
                  for names in itertools.combinations(names_list, i)]
variable_names = pd.Series(variable_names)
# errors = np.apply_along_axis(Linear_Regression, 0, variable_names)
errors = variable_names.apply(Linear_Regression)


############################################
### Question 2 #############################
############################################
### Part 1 #################################

n = 200; p = 100
## Training set
X = np.random.multivariate_normal(np.zeros(p), np.identity(p), n)
y = np.sum(X, axis = 1) + np.random.normal(size=n)
    
