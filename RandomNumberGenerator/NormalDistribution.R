#Dongzhe Li 104434089
#normal distribution 
library (Rcpp)
num = 0
iter = 10000
sourceCpp("/Users/Dongzhe/Desktop/github/Statistical-Prgramming/RandomNumberGenerator/NormalDistribution.cpp")

#result_all is to store all the output and only need to set n as half
result_all=rnormC(7000, iter/2)
hist(result_all, breaks = 30, prob=T, xlab = "Z",main = paste("Histogram of normal distribution"))
curve(dnorm(x,mean=0,sd=1), lwd=2, add=T, col="blue")

