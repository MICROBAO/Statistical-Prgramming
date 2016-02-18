#read the data from csv
Data=read.csv('/Users/Dongzhe/Desktop/stat 202a/state_x77.csv')#already a data frame
str(Data)
#the data has already trimmed to deal with 

summary(Data)
#take some of the attributes from the data
X=Data[c("Population","Income","Illiteracy","Murder","HS.Grad","Frost","Density")]
Y=Data["Life.Exp"]
row = nrow(X)
A = cbind(rep(1, row), X, Y)



mylm_qr<-function(Z){
  n = nrow(Z)
  m = ncol(Z)
  R = Z
  Q = diag(n)
  
  for (k in 1:(m-1))
  {
    x = matrix(rep(0, n), nrow = n)
    x[k:n, 1] = R[k:n, k]
    g = sqrt(sum(x^2))
    v = x
    v[k] = x[k] + sign(x[k,1])*g
    s = sqrt(sum(v^2))
    if (s != 0)
    {
      u = v / s
      R = R - 2 * u %*% t(u) %*% as.matrix(R)
      Q = Q - 2 * u %*% t(u) %*% as.matrix(Q)
    }
  }
  result=list(t(Q), R)
  names(result)=c("Q", "R")
  R1 = R[1:(m-1), 1:(m-1)]
  Y1 = R[1:(m-1), m]
  #return the coefficients only
  beta = solve(R1, Y1)
  beta
}


model1 = lm(Life.Exp ~ Population+Income+Illiteracy+Murder+HS.Grad+Frost+Density,data=Data)
coef(model1)
summary(model1)
mylm_qr(A)
#compare the result between the linear model and the qr result. The function is correct!

#(Intercept)    Population        Income    Illiteracy        Murder       HS.Grad
#7.094143e+01  6.248966e-05  1.484507e-04  1.451796e-01 -3.319248e-01  3.746457e-02
#Frost       Density
#-5.532853e-03 -7.995441e-04
#rep(1, row)    Population        Income    Illiteracy        Murder       HS.Grad 
#7.094143e+01  6.248966e-05  1.484507e-04  1.451796e-01 -3.319248e-01  3.746457e-02 
#Frost       Density 
#-5.532853e-03 -7.995441e-04 


mylm_sweep<-function(A, m)
{
  n <- dim(A)[1]
  
  for (k in 1:m) 
  {
    for (i in 1:n)     
      for (j in 1:n)   
        if (i!=k  & j!=k)     
          A[i,j] <- A[i,j] - A[i,k]*A[k,j]/A[k,k]    
    
    for (i in 1:n) 
      if (i!=k) 
        A[i,k] <- A[i,k]/A[k,k]  
    
    for (j in 1:n) 
      if (j!=k) 
        A[k,j] <- A[k,j]/A[k,k]  
    
    A[k,k] <- - 1/A[k,k] 
  }
  beta = A[1:m, m+1]
  beta
}

X=Data[c("Population","Income","Illiteracy","Murder","HS.Grad","Frost","Density")]
Y=Data["Life.Exp"]
col = ncol(X)
A = cbind(rep(1, row), X, Y)
Z=t(A)%*%as.matrix(A)
#this time input matrix is the a'a

mylm_sweep(Z,col+1)
summary(model1)
#The function is correct

#rep(1, row)    Population        Income    Illiteracy        Murder       HS.Grad 
#7.094143e+01  6.248966e-05  1.484507e-04  1.451796e-01 -3.319248e-01  3.746457e-02 
#Frost       Density 
#-5.532853e-03 -7.995441e-04 
