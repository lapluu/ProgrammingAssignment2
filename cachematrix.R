## makeCacheMatrix and cacheSolve are pair of functions that
## caches the inverse of a matrix. 
## first, call makeCacheMatrix to create a default square
## matrix or you can specify the dimension of the square matrix.
## Then, you
## invoke the cacheSolve(makecm) to compute the inverse of the
## matrix. 
##    If you call the cacheSolve(makecm) second time, it will
## return the inverse matrix in the cache. 
##    You can also assign an invertable to the makeCacheMatrix

makeCacheMatrix <- function( m = NULL, dimension = 2) {
  ## This function make a sqaure matrix with
  ## dimension and stores the matrix in its own cache.
  ## In addition, this function provide method
  ## to set and get the inverse matrix 
  ## 
  ## default dimension
  if(is.null(m)){
    set.seed(99)
    if(dimension > 1){
      m <- matrix(rnorm(dimension^2), dimension, dimension)
    }else{
      m <- matrix()
    }
  }
  invm <- NULL
  ## set the matrix m to y and null the inverse
  set <- function(y){
    m <<- y
    invm <<- NULL
  }
  ## Return the matrix m
  get <- function()  m
  setInverse <- function(inv){
    invm <<- inv
  } 
  getInverse <- function(){
    invm
  }
  ## return a square matrix with random numbers
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of a special "matrix"
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve 
## will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...){
  
  ## Does the matrix had been changed ?
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Retrieve cached inverse matrix")
    return(inv)
  }
  ## Need to compute an inverse
  message("Compute inverse matrix")
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  return(inv)
}
