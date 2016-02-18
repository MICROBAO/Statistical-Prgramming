
#Dongzhe Li 104434089
#stat hw6
library(data.table) # allows us to use function fread,

accuracy <- function(p, y) 
{
  return(mean((p==1) == (y == 1)))
}


myAdaboost=function(X,Y,Xtest,Ytest,num_iterations=10)
{
  n=dim(X)[1]
  p=dim(X)[2]
  #set the threshold as 0.8 at first and this is the weak classifier(base learner)
  th=.8
  X1=X>th
  X1=2*X1-1
  Y=Y*2-1
  beta=matrix(rep(0,p),nrow=p)
  
  Xtest1=Xtest>th
  Xtest1=2*Xtest1-1
  Ytest=Ytest*2-1
  #for each iteration change the beta based on the error
  for(it in 1:num_iterations)
  {
    #no intercept
    score=X1%*%beta
    
    w=exp(-Y*score)
    a=matrix(rep(1,n),nrow=1)%*%(matrix(w*Y,n,p)*X1)/n
    e=(1-a)/2
    j=which.min(e)
    beta[j]=beta[j]+log((1-e[j])/e[j])/2
    
  }
  
  predict_training=sign(X1%*%beta)
  training_acc=mean(predict_training*Y)
  #training_acc=accuracy(predict_training,Y)
  
  predict_test=sign(Xtest1%*%beta)
  test_acc=mean(predict_test*Ytest)
  #test_acc=accuracy(predict_test,Ytest)
  
  cat("Final traning accuracy: ",training_acc,"%","\nFinal testing accuracy: ",test_acc,"%")
}


load_digits <- function(subset=NULL, normalize=TRUE) {
  
  #Load digits and labels from digits.csv.
  
  #Args:
  #subset: A subset of digit from 0 to 9 to return.
  #If not specified, all digits will be returned.
  #normalize: Whether to normalize data values to between 0 and 1.
  
  #Returns:
  #digits: Digits data matrix of the subset specified.
  #The shape is (n, p), where
  #n is the number of examples,
  #p is the dimension of features.
  #labels: Labels of the digits in an (n, ) array.
  #Each of label[i] is the label for data[i, :]
  
  # load digits.csv, adopted from sklearn.
  
  df <- fread("/Users/Dongzhe/Desktop/stat 202a/hw6/digits.csv") 
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
  
  # convert to numpy arrays.
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

result = load_digits(subset=c(1, 2), normalize=TRUE)
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

X=training_digits 
Y=training_labels

#train the digits and output the training and testing accuracy
myAdaboost(training_digits,training_labels,testing_digits,testing_labels)


