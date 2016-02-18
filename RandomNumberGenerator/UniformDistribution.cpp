#include <Rcpp.h>
using namespace Rcpp;


/* Generate random uniform numbers using the linear congruential method */
// [[Rcpp::export]]
NumericVector runifC(double seed, int n) {
  
  /* This function takes in 2 inputs:
  seed: This is the seed used to generate the random numbers
  n: This is the number of random numbers we want to generate
  */
  
  double x = seed; // Create a variable x of type double. Initialize its value to seed.
  NumericVector u(n);  // Create a vector u of length n. This is the vector of random uniform
  // numbers we will be generating
  
  for(int i = 0; i < n; ++i){                                      
    x = fmod(pow(7,5) * x, (pow(2,31) - 1.0));    
    u[i] = x / (pow(2,31) - 1.0);   // Normalize u[i] so that the values lie in [0, 1]
  }                                 
  return(u); 
}
