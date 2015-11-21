## makeCacheMatrix and cacheSolve are pair of functions that
## caches the inverse of a matrix. 
## first, you have to get an instance of the makeCacheMatrix
## as makecm <- makeCacheMatrix(). Then You can create a square
## matrix by yourself and uses the makecm$setMatrix() to store
## the matrix into the cache. Optionally makecm$createMatrix()
## provides a convenience function to create a square method by
## supplying the desired dimension as argument. Then, you
## invoke the cacheSolve(makecm) to compute the inverse of the
## matrix. 
##    If you call the cacheSolve(makecm) second time, it will
## return the inverse matrix in the cache. 
##    If you store a new matrix into the makecm, by calling
## makecm$setMatrix(), the matrix version counter will increment
## by one to indicate that the matrix has been changed. 
## If you call the cacheSolve(makecm) again, cacheSolve(makecm)
## detect that the version of the inverse matrix is NOT matched
## to the version of matrix;therefore, it will compute a new
## inverse matrix and returns this new inverse version instead.

## makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse. Both the matrix and its inverse
## have version counter to keep track of versioning.

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
  ## optional function to create a square matrix
  ## with random values for ease of testing purpose
  ## The original assigment requirement does not
  ## have this requirement
  createMatrix <- function(dimension =2){
    if(dimension == 0){
      return(m)
    }
    set.seed(99)
    m <- matrix(rnorm(dimension^2), dimension, dimension)
  }
  setInverse <- function(inv){
    invm <<- inv
  } 
  getInverse <- function(){
    invm
  }
  ## return a square matrix with random numbers
  list(set = set, 
       get = get,
       createMatrix = createMatrix,
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
