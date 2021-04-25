## Samer Sheich Essa



## The function lsq solves for transpose(A) with vector y 
## if and only if the following conditions applies:
# neither X nor y have the length of 0
# X is a numerical Matrix.
# y is a numerical Matrix with a column or a numerical atomic Vector.
# neither X nor y hold any NA values
# The dimensions of X and Y are compatible, in a way such that t(X) %*% y is calculable.
# t(X) %*% X is invertible. 
#


lsq <- function(X, y) {
  # TODO
  typo1 <- typeof(X)
  typo2 <- typeof(y) 
  dimi <- dim(X)[1] ## number of rows of the matrix
  vec_length = length(y)
  y_col_dim = dim(y)[2]
  
  X_na = sum(is.na(X)) ## check if there is na values in matrix X
  y_na = sum(is.na(y)) ## check if there is ny values in vector y
  
  check_invertible <- function(m) class(try(solve(m),silent=T))=="matrix"
  
  
  stopifnot("matrix is containg none integer values " = typo1 != "character", typo2 != 'character')
  stopifnot("matrix is containg none integer values " = typo1 != "list",
            " y is a list and not a vector/matrix" = typo2 != 'list')
  stopifnot("the length of X is 0" = length(X) >0, "the length of y is 0" = length(y) >0)
  
  stopifnot("y must be either numerical vector with one column or numerical atomic vector" = y_col_dim == 1)
  
  stopifnot("Can't do matrix multiplication on zero dimension vector " = is.null(dim(X)[1]) != TRUE)
  stopifnot("number of rows in X is not equal to length of y vector" = dimi == vec_length)
  stopifnot("NA values are found in the Matrix X" = X_na == 0, 
            "Na values are found in y" = y_na == 0)
  
  A <- t(X) %*% X
  stopifnot("the matirx A is not invertible " = check_invertible(A) ==TRUE)
  
  solve(A, t(X) %*% y) 
}

lsq(matrix(1:6, nrow=3), 1:3)
lsq(matrix(runif(6), nrow=3), matrix(runif(3), ncol=1))
lsq(matrix(letters[1:6] , nrow=2), 1:3) ##
lsq(matrix(1:6, nrow=3), list(1,2,3)) ## 
lsq(1:6, 1:3) ## 
lsq(matrix(1:6, nrow=3), array(1:3, dim=c(1,1,3)))
lsq(matrix(1:6, nrow=3), 1:4) ##
lsq(matrix(1:6, nrow=3), matrix(1:3, nrow=1)) ## is should be a column and not one row  
lsq(matrix(1:6, nrow=3), matrix(1:6, nrow=3)) ## numerical vector with one column and not two 
lsq(matrix(double(0), nrow=0, ncol=0), matrix(double(0), nrow=0, ncol=0))
lsq(matrix(1:6, nrow=3), c(1,NA,3)) ## should not cointain NA values..
lsq(matrix(c(1:5, NA), nrow=3), 1:3) ## Na again....
lsq(matrix(c(1,1,2,1,1,2), nrow=3), 1:3) ## 

