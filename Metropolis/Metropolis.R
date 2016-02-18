#DongzheLi 104434089
#metroplis 
#First we create a function program for f(x)
library(Rcpp)
sourceCpp("/Users/Dongzhe/Desktop/github/Statistical-Prgramming/Metropolis/Metropolis.cpp")
#The two support functions are also written in c++, which can speed up
N=10^3
T = 10^2 
#the input chain is a matirx
chain=matrix(0,1,N) 

chain=Metropolis(chain,N, T,4000)

hist(chain,xlim=range(-10,10),freq=FALSE)
