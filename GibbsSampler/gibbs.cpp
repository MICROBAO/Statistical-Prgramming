#include <Rcpp.h>
using namespace Rcpp;


NumericVector runifC(double seed, int n) {
  
  double x = seed; // Create a variable x of type double. Initialize its value to seed.
  NumericVector u(n);  // Create a vector u of length n. This is the vector of random uniform
  // numbers we will be generating
  
  for(int i = 0; i < n; ++i){                                      
    x = fmod(pow(7,5) * x, (pow(2,31) - 1.0));   
    u[i] = x / (pow(2,31) - 1.0);   // Normalize u[i] so that the values lie in [0, 1]
  }                           
  
  return(u); 
}
/* Generate standard normal random variables using the polar transformation method */

NumericVector rnormC(double seed, int n) {
  
  /* This function takes in 2 inputs:
  seed: This is the seed used to generate the random numbers
  n: This is HALF the number of random numbers we want to generate.
  Since this method generates an (X, Y) pair jointly, each iteration will
  output 2 standard normal variables (so n iterations generated 2n random numbers). 
  Of course, you can change this if you want.
  */
  
  double pi = 3.141592653589793238462643383280; // You can declare this globally as well
  double theta; 
  double r;
  NumericVector u(2);     // Create a vector u of length 2.
  NumericVector v(2 * n); // Create a vector v of length 2 * n. This vector stores our output.
  
  for(int i = 0; i < n; ++i){
    
    u = runifC(seed * (i + 1), 2); // Generate 2 random uniform numbers. Note: the first input 
    // changes on every iteration so we don't constantly generate 
    // the same random numbers. 
    
    theta = 2.0 * pi * u[0];  // u[0] is the first random number we generated
    r = sqrt(-2 * log(u[1])); // u[1] is the second random number we generated
    
    v[2 * i] = r * cos(theta) ; 
    v[(2 * i) + 1] = r * sin(theta);      
  }
  
  return(v);  // This means we want our function to output the vector v.
  
}

// [[Rcpp::export]]

void gibbs_samp(NumericMatrix p1,NumericMatrix p2,int T=1000,int  M=100, double rho=0.9, double x0=0,double y0=0,double seed=4000) 
  {
  NumericVector v(2);
  //genearte two vectors for norm distribution samples
  for(int i =0;i<M;i++){
  
    double x=x0;
    double y=y0;
    p1(0,i)=x;
    p2(0,i)=y;
    
    for(int j=1;j<T;j++){
      v=rnormC(seed*(i+1), 1);
      //this part is the bivariate distribution of x and y
      x=rho*x+sqrt(1-pow(rho,2))*v[0];

      y=rho*x+sqrt(1-pow(rho,2))*v[1];
      
      p1(j,i)=x;
      p2(j,i)=y;
    }
  }
  }
