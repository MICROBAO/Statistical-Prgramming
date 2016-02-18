#DongzheLi 104434089
#Gibbs sampler
library(Rcpp)
sourceCpp("/Users/Dongzhe/Desktop/github/Statistical-Prgramming/GibbsSampler/gibbs.cpp")
T=1000
M=100
rho <- 0.9
p<-rep(0,2*M*T)
p1<-rep(0,M*T) #Allocate memory for results
p2<-rep(0,M*T)
dim(p1)<-c(T,M)
dim(p2)<-c(T,M)
#p1 and p2 are T*M matrix

#the gibbs sampler function is written in c++
#it requires 2 input one is Xt--p1 another one is Yt--p2
#each of them have M columns which represent m parallel chains 
# we set start point as (5,-8) to see the change of (Xt,Yt) over time
p=gibbs_samp(p1,p2,M=M,rho=rho,x0=5,y0=-8) 


install.packages("animation")
#making a movie
library(animation)
par(mar=c(2,2,1,2), mfrow=c(3,3))
lims <- 8*c(-1,1)
#draw the change of the satterplot at 10 different time 
#from the plots, we can see that the relationship between Xt and Yt is roughly linear 
#and cluster is slowly moving towards the right cover, which can be easliy explained by 
# the moving people model
for (t in 1:10){ plot(p1[t,],p2[t,],
                      xlim=lims, ylim=lims,
                      col=1:M,
                      pch=16, main=paste('t =',t))
  ani.pause(.2)
}
