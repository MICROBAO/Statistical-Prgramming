#Dongzhe Li 104434089 
# coding: utf-8
# Logistic regression with weighted least squares, which is based on linear regression function, 
# which in turn is based on QR
# In[48]:

import numpy as np
from scipy import linalg
import numpy as np
from scipy import linalg
import pandas as pd
import warnings
warnings.simplefilter('ignore', DeprecationWarning)

data = pd.read_csv("http://www.ats.ucla.edu/stat/data/binary.csv")
data.head()


# In[49]:

X=data.get(["gre","gpa","rank"])
Y=data.get(["admit"])
nx, mx = X.shape

beta = np.zeros((mx, 1))
epsilon = 1e-6
beta


# In[50]:

def exp_it(_x):
    x = _x.copy()
    y = 1 / (1 + np.exp(-x))
    return y


# In[51]:

def mylm_qr(A):
    n, m = A.shape
    R = A.copy()
    Q = np.eye(n)

    for k in range(m-1):
        x = np.zeros((n, 1))
        x[k:, 0] = R[k:, k]
        v = x
        v[k] = x[k] + np.sign(x[k,0]) * np.linalg.norm(x)

        s = np.linalg.norm(v)
        if s != 0:
            u = v / s
            R -= 2 * np.dot(u, np.dot(u.T, R))
            Q -= 2 * np.dot(u, np.dot(u.T, Q))
    Q = Q.T
    R1 = R[:m-1, :m-1]
    Y1 = R[:m-1, m-1]
    beta = np.linalg.solve(R1, Y1)
    beta = pd.DataFrame(beta)
    #make the beta a dataframe , this will sovle the problem that when y-pr the data type is diffent 
    return beta



# In[54]:
#my logistic func using qr
def mylogistic(_x, _y):
    x = _x.copy()
    y = _y.copy()
    r, c = x.shape

    beta = np.zeros((c, 1))
    epsilon = 1e-6

    while True:
        eta = np.dot(x, beta)
        pr = exp_it(eta)
        w = pr * (1 - pr)
        z = eta + (y - pr) / w
        sw = np.sqrt(w)
        mw = np.repeat(sw, c, axis=1)

        x_work = mw * x
        y_work = sw * z
        rw,cw=x_work.shape
        A = pd.concat([x_work, y_work], axis=1)
        a=A.as_matrix(columns=None)

        beta_new=mylm_qr(a)
        #beta_new, _, _, _ = np.linalg.lstsq(x_work, y_work)
        
        err = np.sum(np.abs(beta_new - beta))
        beta = beta_new
        #beta here is a dataframe so the comparison must be specific 
        if err[0] < epsilon:
            break

    return beta


# In[55]:

if __name__ == '__main__':
    A = pd.concat([X, Y], axis=1)
    logistic_beta = mylogistic(X, Y)
    print logistic_beta


# In[56]:

#The Result without intercept
#           0
# 0  0.001477
# 1 -0.004167
# 2 -0.669538


# In[59]:

intercept =pd.DataFrame(np.ones((nx, 1)))
A = pd.concat([intercept, X], axis=1)
logistic_beta = mylogistic(A, Y)
print logistic_beta


# In[60]:

from sklearn import linear_model
logreg = linear_model.LogisticRegression(C=1e5)
logreg.fit(X,Y)


# In[61]:

#compare the result with builtin logistic regression model 
print(logreg.intercept_[0],logreg.coef_[0][0],logreg.coef_[0][1],logreg.coef_[0][2])
#(-3.4182971433882496, 0.0022753758551117887, 0.77052805849734396, -0.55881025168981058)
#the result with the intercept is similar

# In[ ]:



