#Dongzhe Li 104434089
#uniform distribution 
install.packages("Rcpp")
library (Rcpp)
sourceCpp("/Users/Dongzhe/Desktop/github/Statistical-Prgramming/RandomNumberGenerator/UniformDistribution.cpp")
length=10000#generate 10000 samples of normal distribution
z <- rep(0,length)
z=runifC(1,10000)#call the c++ function, set seed=1
hist(runifC(1,10000),xlab ="U",prob=T,main="Histogram of U")
  
  