library (Rcpp)
my_ridge <- function(X, Y, lambda)
{
  n = dim(X)[1]
  p = dim(X)[2]
  Z = cbind(rep(1, n), X, Y)
  A = t(Z) %*% Z
  D = diag(rep(lambda, p+2))
  D[1, 1] = 0
  D[p+2, p+2] = 0
  A = A + D
  S = mySweep_cpp(A, p+1)
  beta = S[1:(p+1), p+2]
  return(beta)
}

sourceCpp("SweepOperator.cpp")

n = 40
p = 500
sigma = .1


x = runif(n)
x = sort(x)
Y = x^2 + rnorm(n)*sigma
plot(x, Y, col = "red")

my_spline=function(x,Y){
  

num_training <- round(n*0.7)
indices = sample(1:n, size = n)
training_idx <- indices[1:num_training]
testing_idx <- indices[-(1:num_training)]
x_train=x[training_idx]
x_test=x[testing_idx]
Y_train=Y[training_idx]
Y_test=Y[testing_idx]




X_train = matrix(x_train, nrow=num_training )
for (k in (1:(p-1))/p)
  X_train = cbind(X_train, (x_train>k)*(x_train-k))

#  beta = my_ridge(X, Y, lambda)
#  Yhat = cbind(rep(1, n), X)%*%beta
num_test=length(x_test)
X_test = matrix(x_test, nrow=num_test )
for (k in (1:(p-1))/p)
  X_test= cbind(X_test, (x_test>k)*(x_test-k))



#lambda=seq(0, by = 0.01, length =250 )

test_err=rep(0,250)

# par(new = TRUE)

lambda=seq(0, by = 0.01, length =250 )

training_err=rep(0,250)
#mean((Y-Yhat)^2)

for (i in 1:250)
{
  beta = my_ridge(X_train, Y_train, lambda[i])
  Yhat_train = cbind(rep(1, num_training), X_train)%*%beta
  training_err[i]=mean((Y_train-Yhat_train)^2)
  
  Yhat_test = cbind(rep(1, num_test), X_test)%*%beta
  test_err[i]=mean((Y_test-Yhat_test)^2)
  
  if(i%%25==0){
    cat(i,training_err[i],test_err[i])
  }
  
}
plot(lambda, test_err,type="l", col = "blue")
par(new = TRUE)
plot(lambda, training_err,type="l" ,col = "red")
}
#draw the test and train err over differnt lambda
my_spline(x,Y)

X = matrix(x, nrow=n)
for (k in (1:(p-1))/p)
  X = cbind(X, (x>k)*(x-k))

plot(x, Y, ylim = c(-.2, 1.2), col = "red")

#polt the yhat~x for different lambda
lambda_test=c(1,0.1,0.01)
color=c("green","yellow","blue")
for(i in 1:3)
{
  par(new = TRUE)
  
  beta = my_ridge(X, Y, lambda_test[i])
  Yhat = cbind(rep(1, n), X)%*%beta
  plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = color[i])
  
}