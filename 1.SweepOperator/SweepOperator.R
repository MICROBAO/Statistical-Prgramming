
#the optional is done except for the determinent is not very effeicient and 
#the recursion version is easy to code but with more time complexity 

#dongzheli 104434089
#sweep operator ---fortified with input restricts : square invertible matrix

mySweep<-function(A, m){
  
  row=dim(A)[1]
  col=dim(A)[2]
  #chenk wether the row equals to col 
  if(row!=col)
    {
    print("The input matrix is not a square matrix!")
    return(A)
  }
  
  
  #A square matrix is singular if and only if its determinant is 0
  #check whether the matrix's determinant is 0
  if(det(A)==0){
    print("The input matrix  is not invertible !")
    return (A)
  }
  
  
  for (k in 1:m){
    for (i in 1:row){
      for (j in 1:row)   
        if (i!=k  & j!=k)     
          A[i,j] = A[i,j] - A[i,k]*A[k,j]/A[k,k]    
    }
      for (i in 1:row) 
        
        if (i!=k) 
          A[i,k] = A[i,k]/A[k,k]  
      
      for (j in 1:row) 
        if (j!=k) 
          A[k,j] = A[k,j]/A[k,k]  
      
      A[k,k] = - 1/A[k,k] 
    
    
  }
  return(A)
}
  
#check mySweep(A,3)=-solve(A)
A = matrix(c(1,2,3,7,1,2,1,2,2), 3,3)
solve(A)
mySweep(A,3)


#fortified Guass-Jordan elimination 
myGaussJordan <- function(A, m)
{
  
  row=dim(A)[1]
  col=dim(A)[2]
  #chenk wether the row equals to col 
  if(row!=col)
  {
    print("The input matrix is not a square matrix!")
    return(A)
  }
  
  
  #A square matrix is singular if and only if its determinant is 0
  #check whether the matrix's determinant is 0
  if(det(A)==0){
    print("The input matrix  is not invertible !")
    return (A)
  }
  n = dim(A)[1]
  B = cbind(A, diag(rep(1, n)))
  
  for (k in 1:m) 
  {
    a <- B[k, k]
    for (j in 1:(n*2))     
      B[k, j] <- B[k, j]/a
    for (i in 1:n)
      if (i != k)
      {
        a <- B[i, k]
        for (j in 1:(n*2))
          B[i, j] <- B[i, j] - B[k, j]*a; 
      }    
  }
  return(B)
}


A = matrix(c(1,2,3,7,11,13,17,21,23), 3,3)
solve(A)
myGaussJordan(A,3)

#fortified vector version of Guass-Jordan elimination
myGaussJordanVec <- function(A, m)
{
  
  row=dim(A)[1]
  col=dim(A)[2]
  #chenk wether the row equals to col 
  if(row!=col)
  {
    print("The input matrix is not a square matrix!")
    return(A)
  }
  
  
  #A square matrix is singular if and only if its determinant is 0
  #check whether the matrix's determinant is 0
  if(det(A)==0){
    print("The input matrix  is not invertible !")
    return (A)
  }
  n = dim(A)[1]
  B = cbind(A, diag(rep(1, n)))
  
  for (k in 1:m) 
  {
    B[k, ] <- B[k, ]/B[k, k]
    for (i in 1:n)
      if (i != k)
        B[i, ] <- B[i, ] - B[k, ]*B[i, k];   
  }
  return(B)
}

A = matrix(c(1,2,3,7,11,13,17,21,23), 3,3)
solve(A)
myGaussJordanVec(A,3)



#optinal 2 find the detminant by using sweep operator
Find_Det<-function(A){
  
  row=dim(A)[1]
  col=dim(A)[2]
  
  if(row!=col)
  {
    print("The input matrix is not a square matrix!")
    return(A)
  }
  k=row-1
  A11=matrix(1:(2*k),k,k)
  A12=matrix(1:k,k,1)
  A21=matrix(1:k,1,k)
  A22=matrix(0)
  
  for(i in 1:k){
    for (j in 1:k){
      A11[i,j]=A[i,j]
      
    }
  }
  for (i in 1:k){
    A12[i,1]=A[i,row]
  }
  
  for (j in 1:k){
    A21[1,j]=A[row,j]
  }
  A22[1,1]=A[row,col]
  
  result=det(A11)*det(A22+A21%*%mySweep(A11,k)%*%A12)
  return(result)
}

det(A)
Find_Det1(A)


#optional 3 reverse the sweep operator 
my_reverse_Sweep<-function(A, m){
  
  row=dim(A)[1]
  col=dim(A)[2]
  #chenk wether the row equals to col 
  if(row!=col)
  {
    print("The input matrix is not a square matrix!")
    return(A)
  }
  
  
  #A square matrix is singular if and only if its determinant is 0
  #check whether the matrix's determinant is 0
  if(det(A)==0){
    print("The input matrix  is not invertible !")
    return (A)
  }
  
  
  for (k in 1:m){
    for (i in 1:row){
      for (j in 1:row)   
        if (i!=k  & j!=k)     
          A[i,j] = A[i,j] - A[i,k]*A[k,j]/A[k,k]    
    }
    for (i in 1:row) 
      
      if (i!=k) 
        A[i,k] = -A[i,k]/A[k,k]  
    
    for (j in 1:row) 
      if (j!=k) 
        A[k,j] = -A[k,j]/A[k,k]  
    
    A[k,k] = - 1/A[k,k] 
    
    
  }
  return(A)
}

#check reverse sweep operator
A = matrix(c(1,2,3,7,1,2,1,2,2), 3,3)
#solve(A)
print("sweep opertor SWP(A,1)")
mySweep(A,1)
print ("reverse sweep operator SWP(A,1)")
my_reverse_Sweep(A,1)
print("reverse swp operator on swp operator is equal to A")
my_reverse_Sweep(mySweep(A,1),1)

