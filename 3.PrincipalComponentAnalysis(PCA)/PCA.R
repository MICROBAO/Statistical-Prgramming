
#Dongzhe Li 104434089 
#Principal component analysis. The input is a data set in the form of an nxp matrix X.
#The output includes the principle components Q and the corresponding variances Lambda

my_pca=function(X)
{
 # nx=nrow(X)
 # A=t(X)%*%X/n
  A=cov(X)
  T=1000
  n=nrow(A)
  V=matrix(rnorm(n*n),nrow=n)

  for(i in 1:T)
  {
    #V=myqr(V)$Q
    nv = nrow(V)
    mv = ncol(V)
    R=V
    Q = diag(nv)
    
    for (k in 1:(mv-1))
    {
      x = matrix(rep(0, nv), nrow = nv)
      x[k:nv, 1] = R[k:nv, k]
      g = sqrt(sum(x^2))
      v = x
      v[k] = x[k] + sign(x[k,1])*g
      
      s = sqrt(sum(v^2))
      if (s != 0)
      {
        u = v / s
        R = R - 2 * u %*% t(u) %*% R
        Q = Q - 2 * u %*% t(u) %*% Q
      }
    }
    V=t(Q)
    V=A%*%V
  }
  
  nb = nrow(V)
  mb = ncol(V)
  R = V
  Q = diag(nb)
  
  for (k in 1:(mb-1))
  {
    x = matrix(rep(0, nb), nrow = nb)
    x[k:nb, 1] = R[k:nb, k]
    g = sqrt(sum(x^2))
    v = x
    v[k] = x[k] + sign(x[k,1])*g
    
    s = sqrt(sum(v^2))
    if (s != 0)
    {
      u = v / s
      R = R - 2 * u %*% t(u) %*% R
      Q = Q - 2 * u %*% t(u) %*% Q
    }
  }
  
  
  result=list(t(Q),diag(R))
  names(result)=c("vector","values")
  return (result)
}

#take 4 vars
n=100
p=5
X=iris[-5]

A=cov(X)
#only when X's mean is 0 then we can use X%*%X/n

my_pca(X)
eigen(A)$values
sqrt((n-1)/n*my_pca(X)$values)
#there is an n-1/n difference between the real value and the calculated pca
princomp(X)

