# -*- coding: utf-8 -*-
"""
Created on Wed Sep 27 23:52:52 2017

@author: whuang67
"""

import numpy as np

# Question 1
P, N, rho = 4, 200, 0.5
V = np.array([[1, .5, .25, .125],
              [.5, 1, .5, .25],
              [.25, .5, 1, .5],
              [.125, .25, .5, 1]])


np.random.seed(1)
X = np.random.multivariate_normal(np.repeat(0, P), V, N)
beta = np.array([1, 1, .5, .5])
y = np.matmul(X, beta) + np.random.normal(0, 1, N)

### a
X_ = X-np.mean(X, axis=0)
sigma = np.matmul(X_.T, X) / X_.shape[0]
print(sigma)

### b
def mydist(arr_1, arr_2):
    return (np.sum((arr_1 - arr_2)**2))**.5

distance = np.apply_along_axis(mydist, 1, X, np.array([.5, .5, .5, .5]))
print(y[np.argsort(distance)[0:5]].mean())

### c
def mydist2(arr_1, arr_2, s):
    return np.matmul(np.matmul(np.transpose(arr_1-arr_2), np.linalg.inv(s)), 
                    (arr_1-arr_2))

distance = np.apply_along_axis(mydist2, 1, X, np.array([.5, .5, .5, .5]), sigma)
print(y[np.argsort(distance)[0:5]].mean())

### d

# Question 2
### b
# Generate X
X = np.random.multivariate_normal([0., 0., 0., 0.],
                                  [[1., 0., 0., 0.],
                                   [0., 1., 0., 0.],
                                   [0., 0., 1., 0.],
                                   [0., 0., 0., 1.]],
                                   200)

# True function f(X)
f_X = np.matmul(X, np.array([1., 2., 3., 4.]))

# Add noise
np.random.seed(1)
y = f_X + np.random.normal(0., 1., 200)

# Repeat 20 times to get a better fit
from sklearn.neighbors import KNeighborsRegressor
df_arr = np.repeat(0., 20)
np.random.seed(1)
y_real = np.zeros(shape=(200, 20)); y_pred = np.zeros(shape=(200, 20))

for i in range(20):
    y_real[:, i] = f_X + np.random.normal(0., 1., 200)
    reg = KNeighborsRegressor().fit(X, y_real[:, i])
    y_pred[:, i] = reg.predict(X)
np.cov(y_real, y_pred)[200:399, 0:199].trace()


### c
y_pred_lr = np.matmul(
        np.matmul(np.matmul(X, np.linalg.inv(np.matmul(X.T, X))), X.T), y)
df_lr = np.cov(y_pred_lr, y).trace()/1.
print(df_lr)


# Question 3
import os
os.chdir("C:/users/whuang67/downloads")
import pandas as pd
SAheart = pd.read_csv("SAheart.csv")
X = SAheart[["age", "tobacco"]]
y = SAheart["chd"]

from sklearn.model_selection import GridSearchCV
from sklearn.neighbors import KNeighborsClassifier
parameter = {"n_neighbors": range(3, 31, 2)}
reg = GridSearchCV(KNeighborsClassifier(),
                   parameter,
                   scoring = "accuracy",
                   cv = 10)
reg.fit(X, y)

cv_result = reg.cv_results_
import matplotlib.pyplot as plt

plt.plot(cv_result["mean_test_score"], label = "CV Error")
plt.plot(cv_result["mean_train_score"], label = "Training Error")
plt.xticks(range(14), range(3, 31, 2))
plt.legend(); plt.xlabel("Neighoors"); plt.ylabel("Accuracy")
plt.title("10-Fold Cross-Validation")
plt.show()
