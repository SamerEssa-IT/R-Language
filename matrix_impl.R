my_matrix <- function(vec, nrow=NULL, ncol=NULL, colnames=NULL, rownames=NULL) {
  # TODO
  length_ <- length(vec)
  check_ <-  FALSE
  if((is.null(ncol) == TRUE) && (is.null(nrow) == TRUE)) check_ <- TRUE
##  stopifnot("at least one of nrow, ncol has to be specified" = check_ == FALSE, 
##            "at least one of nrow, ncol has to be specified llol" = is.null(nrow) == FALSE)

  stopifnot("at least one of nrow, ncol has to be specified" = check_ == FALSE)
  
  if(is.null(nrow) != TRUE && is.null(ncol)){
    #vec <- shape_row(vec,nrow)
    stopifnot(" incompatible length" = length_%%nrow == 0)
    num_col <- length_/nrow
    dim(vec) <- c(nrow, num_col)
    }
  else if((is.null(ncol) == FALSE) && (is.null(nrow)== TRUE)){
    ## check if the length is compatible
    stopifnot(" incompatible length" = length_%%ncol == 0)
    num_row <- length_/ncol
    dim(vec) <- c(num_row, ncol)
    
    
    }
  else {

    dim_shape <- nrow * ncol
    if(vec == 0) {
      vec <- rep(0,dim_shape) 
      length_ <-  dim_shape
      }
    stopifnot("incompatible length" = dim_shape == length_)
    
    dim(vec) <- c(nrow,ncol)
  }
  ## naming cols and vectors: 
  if (is.null(colnames) != TRUE){
    col_names <- c(colnames)
    stopifnot("length of colnames must be ncol" = ncol == length(col_names))
    colnames(vec, do.NULL = FALSE)
    colnames(vec) <- col_names
  }
  if (is.null(rownames) != TRUE){
    row_names <- c(rownames)
    stopifnot("length of colnames must be ncol" = nrow == length(row_names))
    rownames(vec, do.NULL = FALSE)
    rownames(vec) <- row_names
  }
  
  return(vec)
}

my_matrix(1:6)

my_matrix(1:6, ncol=1)
my_matrix(1:6, ncol=2)
cc <- my_matrix(1:6, ncol=3)
my_matrix(1:6, ncol=6)
my_matrix(1:6, ncol=4)
my_matrix(1:6, nrow=2) 
my_matrix(1:6, nrow=7)
my_matrix(1:6, ncol=2, nrow=2) 
my_matrix(1:6, ncol=2, nrow=3) ##
my_matrix(1:6, ncol=2, nrow=1)
my_matrix(0, ncol=3, nrow=2) 
my_matrix(1:6, ncol=3, colnames=LETTERS[1:3])
my_matrix(1:6, ncol=3, colnames=LETTERS[1:2])
my_matrix(1:6, ncol=3, rownames=letters[24 + 1:2]) #### continue here
my_matrix(1:6, ncol=3, colnames=LETTERS[1:3], rownames=letters[24 + 1:2])

?colnames
cc
rownames(cc, do.NULL = FALSE)
cc
rownames(cc) <- LETTERS[1:2]
cc
xx <- LETTERS[1:3]
xx
ccc <- c(xx)
ccc
