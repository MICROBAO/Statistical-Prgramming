
# coding: utf-8


#Dongzheli 104434089

import numpy as np
from scipy import linalg
import pandas as pd
import warnings
warnings.simplefilter('ignore', DeprecationWarning)

data = pd.read_csv('/Users/Dongzhe/Desktop/stat 202a/state_x77.csv')
data.head()

X=data.get(['Population', 'Income', 'Illiteracy','Murder','HS.Grad','Frost','Density' ])
X.head()


Y=data.get(['Life.Exp'])
Y.head()


nx, mx = X.shape
Z = np.hstack((np.ones(nx).reshape((nx, 1)),X,Y))
Z.shape


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
    return beta

print beta


def mylm_sweep(B, m):
    A = np.copy(B)
    n, c = A.shape
    for k in range(m):
        for i in range(n):
            for j in range(n):
                if i != k and j != k:
                    A[i,j] = A[i,j]- A[i,k]*A[k,j]/A[k,k]

        for i in range(n):
            if i != k:
                A[i,k] = A[i,k]/A[k,k]

        for j in range(n):
            if j != k:
                A[k,j] = A[k,j]/A[k,k]

        A[k,k] = -1/A[k,k]
    beta=A[0:m,m]
    return beta

def eigen_qr(A):
    T = 1000
    A_copy = A.copy()
    r, c = A_copy.shape

    V = np.random.random_sample((r, r))

    for i in range(T):
        Q, _ = qr(V)
        V = np.dot(A_copy, Q)

    Q, R = qr(V)

    return R.diagonal(), Q

Zin=np.dot(Z.T,Z)
mylm_sweep(Zin, mx+1)




