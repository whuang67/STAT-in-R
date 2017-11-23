# -*- coding: utf-8 -*-
"""
Created on Tue Nov 21 13:09:06 2017

@author: whuang67
"""

import numpy as np

############################################
### Question 1 #############################
############################################
###### a ###################################
def get_RMSE(y, y_pred): return (np.mean((y-y_pred)**2))**.5

class NadarayaWatsonRegressor:
    def __init__(self, train_X, train_y, test_X, bandwidth=1):
        self.train_X = train_X
        self.train_y = train_y
        self.test_X = test_X
        self.bandwidth = bandwidth
        
    def Gaussian_kernal(self, x1, x2):
        return np.exp(-np.sum(((x1-x2)/self.bandwidth)**2)) / (2*np.pi)**.5
    
    def EachStep(self, test):
        K = np.apply_along_axis(self.Gaussian_kernal, 1, self.train_X, test)
        return (K*self.train_y).sum()/K.sum()
    
    def Get_output(self):
        return np.apply_along_axis(self.EachStep, 1, self.test_X)


n = 200; p = 20
## Training set
X = np.random.multivariate_normal(np.zeros(p), np.identity(p), n)
y = np.sum(X, axis = 1) + np.random.normal(size=n)
## Testing set
X_test = np.random.multivariate_normal(np.zeros(p), np.identity(p), 40)
y_test = np.sum(X_test, axis = 1) + np.random.normal(size=40)
## Predictions
y_pred = NadarayaWatsonRegressor(X, y, X).Get_output()
y_pred_test = NadarayaWatsonRegressor(X, y, X_test).Get_output()
## Performances
print(get_RMSE(y, y_pred))
print(get_RMSE(y_test, y_pred_test))


###### b ###################################
import os
os.chdir("C:/users/whuang67/downloads")

import pandas as pd
dat = pd.read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
dat = dat[["Global_Sales", "Critic_Score", "Critic_Count", "User_Score"]]
for name in ["Global_Sales", "Critic_Score", "Critic_Count", "User_Score"]:
    dat[name] = pd.to_numeric(dat[name], errors = "coerce")
dat.dropna(axis=0, inplace=True)

y = np.log(1+dat["Global_Sales"].values)
X = dat[["Critic_Score", "Critic_Count", "User_Score"]].values


from sklearn.model_selection import KFold
kf = KFold(n_splits = 10); errors = np.array([]); cv_errors = np.array([])
for bandwidth in [1,2,3,4,5]:
    for train_index, test_index in kf.split(X):
        train_X, train_y = X[train_index], y[train_index]
        test_X, test_y = X[test_index], y[test_index]
        pred_y = NadarayaWatsonRegressor(train_X, train_y, test_X, bandwidth).Get_output()
        errors = np.append(errors, get_RMSE(test_y, pred_y))
    cv_errors = np.append(cv_errors, errors.mean())

import matplotlib.pyplot as plt
plt.plot(np.arange(5), cv_errors); plt.show()



############################################
### Question 2 #############################
############################################
###### a ###################################
from sklearn.ensemble import RandomForestRegressor
n = 200; p = 20
X = np.random.multivariate_normal(np.zeros(p), np.identity(p), n)
# y = np.sum(X, axis = 1) + np.random.normal(size=n)

y_real = np.zeros(shape = (n, p)); y_pred = np.zeros(shape = (n, p))
cov_matrix = np.zeros(shape=(4, 4))
mtry = [2, 3, 4, 5]; nodesize = [1, 2, 3, 4]
for i in range(4):
    for j in range(4):
        for k in range(20):
            y_real[:, k] = np.sum(X, axis=1) + np.random.normal(size=n)
            reg = RandomForestRegressor(
                    min_samples_leaf = mtry[i],
                    max_features = nodesize[j]).fit(X, y_real[:, k])
            y_pred[:, k] = reg.predict(X)
        cov_matrix[i, j] = np.cov(y_real, y_pred)[n:2*n-1, 0:n-1].trace()

print(cov_matrix)

###### b ###################################
for i in [10, 20, 30, 40, 100, 500]:
    y_real = np.sum(X, axis=1) + np.random.normal(size=n)
    reg = RandomForestRegressor(n_estimators=i).fit(X, y_real)
    y_pred = reg.predict(X)
    print(np.std(y_pred))



############################################
### Question 3 #############################
############################################
###### a ###################################
def CART(X, y, W=None):
    if W is None:
        W = np.ones(X.shape[0])/X.shape[0]
    def get_score_(cut, y, W):
        left_idx = (X <= cut)
        y_L = y[left_idx]; y_R = y[~left_idx]
        W_L = W[left_idx]; W_R = W[~left_idx]
        p_L = W_L[y_L == 1].sum()/W_L.sum()
        p_R = W_R[y_R == 1].sum()/W_R.sum()
        
        Gini_L = p_L*(1-p_L); Gini_R = p_R*(1-p_R)
        return (W_L.sum()*Gini_L + W_R.sum()*Gini_R)/W.sum()
    
    possible_cuts = np.sort(X)[:-1]
    output = np.array([get_score_(cut, y, W) for cut in possible_cuts])
    best_cut = possible_cuts[np.argmin(output)]
    prediction = np.where(X <= best_cut, 1, -1)
    # apply_along_axis(get_score_, 0, possible_cuts, y, W)
    return {"best_cut": best_cut, "prediction": prediction}


X = np.hstack((np.random.uniform(0, 3, 50), np.random.uniform(2, 5, 50)))
y = np.hstack((np.ones(50), -np.ones(50)))
W = np.ones(100)/100

cut, pred = CART(X, y, W).values()

plt.scatter(X, y); plt.axvline(x=cut); plt.show()


###### b ###################################
def Adaboost(X, y, steps = 100, bootstrap = True):
    weights = np.ones(X.shape[0])/X.shape[0]
    final_predict = np.zeros(y.shape[0])
    
    for i in range(steps):
        if bootstrap is True:
            ### Bootstrap Sampling ###
            idx = np.arange(X.shape[0])
            bootstrap_idx = np.unique(np.random.choice(idx, idx.shape[0]))
            X_, y_, weights_ = X[bootstrap_idx], y[bootstrap_idx], weights[bootstrap_idx]
            cut = CART(X_, y_, weights_)["best_cut"]
            predict = np.where(X <= cut, 1, -1)
            ##########################
        else:
            predict = CART(X, y, weights)["prediction"]
            
        epsilon = np.dot(weights, (predict!=y)*1)
        alpha = .5*np.log((1-epsilon)/epsilon)
        w_ = weights*np.exp(-alpha * y * predict)
        weights = w_/np.sum(w_)
        
        final_predict += alpha*predict
    return np.sign(final_predict)

np.random.seed(1)
n = 300; X = np.random.uniform(size=n)
y = 2*(np.random.binomial(n=1, p=(np.sin(4*np.pi*X)+1)/2, size=n)-0.5)

y_pred1 = CART(X, y)["prediction"]
print(np.mean(y_pred1 == y))
y_pred = Adaboost(X, y, steps = 1000, bootstrap=True)
print(np.mean(y_pred == y))