
T=6000
n = 50
p = 200
s = 10
epsilon=.0001
beta=matrix(rep(0,p),nrow=p)
db=matrix(rep(0,p),nrow=p)
beta_all=matrix(rep(0,p*T),nrow=p)
X = matrix(rnorm(n*p), nrow=n)
beta_true = matrix(rep(0, p), nrow = p)
beta_true[1:s] = 1:s
Y = X %*% beta_true + rnorm(n)

R=Y
for(t in 1:T)
{
  for (j in 1:p)
    db[j]=sum(R*X[j])
  j=which.max(abs(db))
  beta[j]=beta[j]+db[j]*epsilon
  R=R-X[,j]*db[j]*epsilon
  beta_all[,t]=beta
  
}


matplot(t(matrix(rep(1, p), nrow = 1)%*%abs(beta_all)), t(beta_all), type = 'l')
