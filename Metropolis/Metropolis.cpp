#include <Rcpp.h>
using namespace Rcpp;

//this function give the exp() function of pi(x) 
double fx(double x, int variance){return exp(-pow(x,2)/(2*variance));}

//this function is the ratio of pi(y) and pi(x)
//if the ratio is larger than one than it equals to one which is from balance 
//if it is smaller than 1 than all are from y to x
double alpha(double x, double y, int variance=2){
  double ratio=fx(y,variance)/fx(x,variance);
  if (ratio>1)
    return 1;
  else 
    return ratio;
  }
//this is the uniform generator 
NumericVector runifC(double seed, int n) {

  double x = seed; 
  NumericVector u(n);  
  
  for(int i = 0; i < n; ++i){                                      
    x = fmod(pow(7,5) * x, (pow(2,31) - 1.0));     
    u[i] = x / (pow(2,31) - 1.0);  
  }                                 
  return(u);
  
}

// [[Rcpp::export]]
NumericMatrix Metropolis(NumericMatrix C, int N,int T,double seed) {
  for (int i =0;i<N;i++){
    
    NumericVector u(2);
    double U1,U2,x,y;
    x=0;
    for(int j =0;j<T;j++)
    {
      //generate two uniform distribution number u[0],u[1]
      u = runifC(seed * (i + 1), 2);
      U1=u[0];
      //there are 1/2 prob to go either state
      if(U1>0.5)
        y=x+1;
      else
        y=x-1;
      U2=u[1];
      if (U2<alpha(x,y))
        x=y;
    }
    C[i]=x;
  }
  return C;
}

