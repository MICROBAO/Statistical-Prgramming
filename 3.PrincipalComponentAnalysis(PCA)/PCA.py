#Dongzhe Li 104434089
# coding: utf-8
#Principal component analysis. The input is a data set in the form of an nxp matrix X. 
#The output includes the principle components Q and the corresponding variances Lambda

def my_pca(A):
    
    T = 1000
    A_copy = A.copy()
    r, c = A_copy.shape

    V = np.random.random_sample((r, r))

    for i in range(T):
        n, m = V.shape
        R = V.copy()
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
        V = np.dot(A_copy, Q)
    #Q, R = qr(V)
    mv, nv = V.shape
    R = V.copy()
    Q = np.eye(nv)
    
    for k in range(mv-1):
        x = np.zeros((nv, 1))
        x[k:, 0] = R[k:, k]
        v = x
        v[k] = x[k] + np.sign(x[k,0]) * np.linalg.norm(x)
        s = np.linalg.norm(v)
        if s != 0:
            u = v / s
            R -= 2 * np.dot(u, np.dot(u.T, R))
            Q -= 2 * np.dot(u, np.dot(u.T, Q))
    #Q = Q.T
    return R.diagonal(), Q.T



from sklearn import datasets
from sklearn.decomposition import PCA
iris = datasets.load_iris()
X = iris.data[:, :5]
X_reduced = PCA(n_components=4)
X_reduced.fit_transform(X)
X_reduced.explained_variance_
#this is the pca from package sklearn which is based on SVD, the result is similar to our pca function 




X = iris.data[:, :5]
A = np.cov(X.T)

D, V = my_pca(A)
print D.round(6)
print V.round(6)

# Compare the result with the numpy calculation
eigen_value_gt, eigen_vector_gt = np.linalg.eig(A)
print eigen_value_gt.round(6)
print eigen_vector_gt.round(6)






