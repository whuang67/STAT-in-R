# -*- coding: utf-8 -*-
"""
Created on Wed Nov 22 17:51:27 2017

@author: whuang67
"""

############################################
### Question 1 #############################
############################################
import numpy as np
np.random.seed(1)
n = 40; p = 2
xpos = np.reshape(np.random.normal(size=n*p), (n, p))
xneg = np.reshape(np.random.normal(loc=4, size=n*p), (n, p))
x = np.vstack((xpos, xneg))
y = np.hstack((np.ones(n), -np.ones(n)))

###### a ###################################
from sklearn.svm import SVC
clf = SVC(kernel="linear").fit(x, y)
w = np.matmul(clf.dual_coef_, clf.support_vectors_)[0]
b = clf.intercept_
import matplotlib.pyplot as plt
plt.scatter(xpos[:, 0], xpos[:, 1], label = "y=1")
plt.scatter(xneg[:, 0], xneg[:, 1], label = "y=-1")
x_axis = np.array([min(xpos[:, 0]), max(xneg[:, 0])])
y_axis = -x_axis*w[0]/w[1]-b/w[1]
plt.plot(x_axis, y_axis, color="r")
plt.legend(loc="best"); plt.show()


###### b ###################################
