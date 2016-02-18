#Dongzhe li 104434089
#gradient descent learning of logistic regression and two-layer neural network
#moniter the training accuracy and testing accuracy.

library(data.table) # allows us to use function fread,
# which quickly reads data from csv files 

accuracy <- function(p, y) 
{
  return(mean((p > 0.5) == (y == 1)))
}

sigmoid <- function(x){
  return( 1/(1 + exp(-x)))
}
#m effect the speed and acc

train <- function(X, Y, Xtest, Ytest, m=100, num_iterations=2000,
                  learning_rate=1e-1) 
{
  n = dim(X)[1] 
  p = dim(X)[2]+1
  ntest = dim(Xtest)[1]
  
  X1 = cbind(rep(1, n), X)
  Xtest1 = cbind(rep(1, ntest), Xtest)
  
  sigma = .1
  alpha = matrix(rnorm(p*m)*sigma, nrow=p)
  beta = matrix(rnorm(m+1)*sigma, nrow=m+1)
  
  for(it in 1:num_iterations)
    {
    #the first layer
    Z=1/(1+exp(-(X1%*%alpha)))
    Z1=cbind(rep(1,n),Z)
    
    #dimension
    pr=1/(1+exp(-(Z1%*%beta)))
    dbeta=matrix(rep(1,n),nrow=1)%*%(matrix(Y-pr,n,m+1)*Z1/n )
    beta=beta+learning_rate*t(dbeta)
    
    for(k in 1:m)
    {
      #the second layer
      da=(Y-pr)*beta[k+1]*Z[,k]*(1-Z[,k])
      #the first one is intercept
      #z is n*m
      dalpha=matrix(rep(1,n),nrow=1)%*%(matrix(da,n,p)*X1/n )
      alpha[,k]=alpha[,k]+learning_rate*t(dalpha)
    }
    #return(list(beta,alpha))

  model = list(beta,alpha)
  
  #test traning_acc each iter
  Zt=sigmoid(X1%*%model[[2]])
  ntrain = dim(Zt)[1]
  Ztrain1 = cbind(rep(1, ntrain), Zt)
  y_predict=sigmoid(Ztrain1%*%model[[1]])
  train_acc_temp=accuracy(y_predict,Y)
  #print the trannig acc every 100 iter
  if (it %% 100 == 0){
    cat(it,"traning accuracy: ",train_acc_temp,"%\n")
    }
  }
  
  # print the traning acc and test acc at last 
  Ztest=sigmoid(Xtest1%*%model[[2]])
  ntest = dim(Ztest)[1]
  Ztest1 = cbind(rep(1, ntest), Ztest)
  y_predict_test=sigmoid(Ztest1%*%model[[1]])
  test_acc=accuracy(y_predict_test,Ytest)
  cat("\nFinal traning accuracy: ",train_acc_temp,"%","\nFinal testing accuracy: ",test_acc,"%")
  
  #return(model)
}


# load data
load_digits <- function(subset=NULL, normalize=TRUE) {
  
  df <- fread("/Users/Dongzhe/Desktop/stat 202a/hw5/digits.csv") 
  df <- as.matrix(df)
  
  ## only keep the numbers we want.
  if (length(subset)>0) {
    
    c <- dim(df)[2]
    l_col <- df[,c]
    index = NULL
    
    for (i in 1:length(subset)){
      
      number = subset[i]
      index = c(index,which(l_col == number))
    }
    sort(index)
    df = df[index,]
  }
  
  # convert to arrays.
  digits = df[,-1]
  labels = df[,c]
  
  # Normalize digit values to 0 and 1.
  if (normalize == TRUE) {
    digits = digits - min(digits)
    digits = digits/max(digits)}
  
  
  # Change the labels to 0 and 1.
  for (i in 1:length(subset)) {
    labels[labels == subset[i]] = i-1
  }
  
  return(list(digits, labels))
  
}

split_samples <- function(digits,labels) {
  
  # Split the data into a training set (70%) and a testing set (30%).
  
  num_samples <- dim(digits)[1]
  num_training <- round(num_samples*0.7)
  indices = sample(1:num_samples, size = num_samples)
  training_idx <- indices[1:num_training]
  testing_idx <- indices[-(1:num_training)]
  
  return (list(digits[training_idx,], labels[training_idx],
               digits[testing_idx,], labels[testing_idx]))
}



#====================================
# Load digits and labels.
result = load_digits(subset=c(3, 5), normalize=TRUE)
digits = result[[1]]
labels = result[[2]]

result = split_samples(digits,labels)
training_digits = result[[1]]
training_labels = result[[2]]
testing_digits = result[[3]]
testing_labels = result[[4]]

# print dimensions
length(training_digits)
length(testing_digits)

# Train a net and display training accuracy.
train(training_digits, training_labels, testing_digits, testing_labels)
# ntest = dim(testing_digits)[1]
# Xtest1 = cbind(rep(1, ntest), testing_digits)
# Z=sigmoid(Xtest1%*%model[[2]])
# 
# ntest2 = dim(Z)[1]
# Ztest1 = cbind(rep(1, ntest2), Z)
# y_predict=sigmoid(Ztest1%*%model[[1]])
# accuracy (y_predict,testing_labels)
