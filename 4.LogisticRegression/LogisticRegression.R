#qr method lm 
#dongzhe li 104434089 
# Logistic regression with weighted least squares, which is based on linear regression function, 
# which in turn is based on QR
#in this implementation i use qr to calculate beta

mylm_qr=function(Z){
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

expit <- function(x)
{
  y <- 1/(1+exp(-x))
  return(y)
}


library(aod)

mydata=read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)
#mydata$rank <- factor(mydata$rank)

X=mydata[c("gre","gpa","rank")]
Y=mydata["admit"]


myLogistic <- function(X, Y)
{
  n <- nrow(X)
  p <- ncol(X)    
  
  beta <- matrix(rep(0, p), nrow = p)
  epsilon=1e-6
  repeat
  {
    eta <- as.matrix(X)%*%beta
    pr <- expit(eta)
    w <- pr*(1-pr)
    Z <- eta + (Y-pr)/w
    sw <- sqrt(w)
    mw <- matrix(sw, n, p)
    Xwork <- mw*X
    Ywork <- sw*Z
    
    #change it to lmw
    #beta_new <- lm(Ywork ~ 0 + Xwork)$coefficient
    
     row = nrow(Xwork)
     A = cbind( Xwork, Ywork)
    #use lm-qr 
     beta_new=mylm_qr(A)
    
    
    
     err <- sum(abs(beta_new-beta))
     beta <- beta_new
    if (err<epsilon)
       break
  }
  return(beta)
}


row=nrow(X)

myLogistic(cbind(rep(1,row),X), Y)
#compare the result with built_in result 
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

#with the intercept should be fine 
# rep(1, row)         gre         gpa        rank 
# -3.44954840  0.00229396  0.77701357 -0.56003139
# without intercept
# Xworkgre     Xworkgpa    Xworkrank 
# 0.001477072 -0.004166882 -0.669538169
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -3.449548   1.132846  -3.045  0.00233 ** 
#   gre          0.002294   0.001092   2.101  0.03564 *  
#   gpa          0.777014   0.327484   2.373  0.01766 *  
#   rank        -0.560031   0.127137  -4.405 1.06e-05 ***