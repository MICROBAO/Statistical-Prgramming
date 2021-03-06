
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix mySweep_cpp(const NumericMatrix B, int m)
{
  
  
  NumericMatrix A = clone(B);
  int n = A.nrow();
  
  for (int k = 0; k < m;k++){
    for (int i = 0; i < n;i++){
      for (int j = 0; j < n;j++){
        if ((i!=k)  & (j!=k))     
        A(i,j) = A(i,j) - A(i,k)*A(k,j)/A(k,k);    
      }
    }
    
    for (int i = 0; i < n;i++){      
      if (i!=k) 
      A(i,k) = A(i,k)/A(k,k); }  
      
      for (int j = 0; j < n;j++){      
        
        if (j!=k) 
        A(k,j) = A(k,j)/A(k,k); } 
        
        A(k,k) = - 1/A(k,k); 
  }
  
  return A;
}
