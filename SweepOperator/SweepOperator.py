
# coding: utf-8
#the optionals are finished except for the determinent
#dongzhe li 104434089
# In[16]:

import numpy as np
#sweep operator ---fortified with input restricts : square invertible matrix
def mySweep(B, m):
    A = np.copy(B)
    n, c = A.shape
    if n!=c: #chenk wether the row equals to col 
        print "The input matrix is not a square matrix"
        return A
    #A square matrix is singular if and only if its determinant is 0
    #check whether the matrix's determinant is 0
    if np.linalg.det(A)==0:
        print "The input matrix is invertible"
        return A
    
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
    return A

A = np.array([[1,2,3],[7,11,13],[17,21,23]], dtype=float).T
print mySweep(A, 3)
print np.linalg.inv(A)
#note that mySweep(A, 3)=-inverse of A


# In[23]:

#fortified Guass-Jordan elimination calculate inverse of matrix
def myGaussJordan(A, m):
    #n = A.shape[0]
    n, c = A.shape
    if n!=c: #chenk wether the row equals to col 
        print "The input matrix is not a square matrix"
        return A
    #A square matrix is singular if and only if its determinant is 0
    #check whether the matrix's determinant is 0
    if np.linalg.det(A)==0:
        print "The input matrix is invertible"
        return A
    
    
    B  = np.hstack((A, np.identity(n)))
    
    for k in range(m):
        a = B[k, k]
        for j in range(n*2):
            B[k, j] = B[k, j] / a
        for i in range(n):
            if i != k:
                a = B[i, k]
                for j in range(n*2):
                    B[i, j] = B[i, j] - B[k, j]*a;
    return B
A = np.array([[1,2,3],[7,11,13],[17,21,23]], dtype=float).T
print myGaussJordan(A, 3)
print np.linalg.inv(A)


# In[22]:

#fortified vector version of Guass-Jordan elimination to calculate inverse of matrix 
def myGaussJordanVec(A, m):
    n, c = A.shape
    if n!=c: #chenk wether the row equals to col 
        print "The input matrix is not a square matrix"
        return A
    #A square matrix is singular if and only if its determinant is 0
    #check whether the matrix's determinant is 0
    if np.linalg.det(A)==0:
        print "The input matrix is invertible"
        return A
    B  = np.hstack((A, np.identity(n)))
    
    for k in range(m):
        B[k, :] = B[k, ] / B[k, k]
        for i in range(n):
            if i != k:
                B[i, ] = B[i, ] - B[k, ]*B[i, k];
    
    return B

print myGaussJordanVec(A, 3)
print np.linalg.inv(A)


# In[10]:

#optinal 2 find the detminant by using sweep operator I have some problem with this one
def FindDet(A):
    n, c = A.shape
    if n!=c: #chenk wether the row equals to col 
        print "The input matrix is not a square matrix"
        return A
    #assuming it is invertible 
    
    


# In[24]:

#reverse sweep operator ---fortified with input restricts : square invertible matrix
def myReverseSweep(B, m):
    A = np.copy(B)
    n, c = A.shape
    if n!=c: #chenk wether the row equals to col 
        print "The input matrix is not a square matrix"
        return A
    #A square matrix is singular if and only if its determinant is 0
    #check whether the matrix's determinant is 0
    if np.linalg.det(A)==0:
        print "The input matrix is invertible"
        return A
    
    for k in range(m):
        for i in range(n):
            for j in range(n):
                if i != k and j != k:
                    A[i,j] = A[i,j]- A[i,k]*A[k,j]/A[k,k]

        for i in range(n):
            if i != k:
                A[i,k] = -A[i,k]/A[k,k]

        for j in range(n):
            if j != k:
                A[k,j] = -A[k,j]/A[k,k]

        A[k,k] = -1/A[k,k]
    return A
A = np.array([[1,2,3],[7,11,13],[17,21,23]], dtype=float).T

print(myReverseSweep(A,1))
print (mySweep(A, 1))


# In[ ]:



