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

makeCacheMatrix <- function( m = matrix()) {
  ## This function make a square matrix with
  ## dimension and stores the matrix in its own cache.
  ## In addition, this function provide method
  ## to set and get the inverse matrix 
  ## 
  invm <- NULL
  ## increment counter to keep track the version of matrix m
  ## and the version of inverse.
  mCounters <- 0
  iCounters <- 0
  ## Stores the matrix to the cache
  ## and increments the version counter by one.
  setMatrix <- function(y){
    m <<- y
    invm <<- NULL
    mCounters <<- mCounters + 1
    ## print(mCounters)
  }
  ## Returns TRUE if both counters are identical.
  isSync <- function(){
    ## print(mCounters)
    ## print(iCounters)
    if(mCounters == iCounters){
      TRUE
    }else{
      FALSE
    }
  }
  ## Retrieve the matrix from the cache
  getMatrix <- function(){
    ##  print(mCounters)
    m
  } 
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
  ## Stores the inverse of the matrix and
  ## increments the counter by one
  setInverse <- function(inv){
    invm <<- inv
    iCounters <<- iCounters + 1
    ## print(iCounters)
  }
  ## Retrieve the inverse matrix.  
  getInverse <- function(){
    ##  print(iCounters)
    invm
  }
  ## a special patrix with list of methods
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       createMatrix = createMatrix,
       isSync = isSync ,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of a special "matrix"
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve 
## will retrieve the inverse from the cache. 

cacheSolve <- function(x){
   ## Return a matrix that is inverse of x
  mcache <- x$getMatrix()
  if(is.null(mcache)){
    message("NULL matrix data")
    return(mcache)
  }
  ## Does the matrix had been changed ?
  if(x$isSync()){
    inv <- x$getInverse()
    message("getting cached data")
    return(inv)
  }
  ## Need to compute an inverse
  message("creating new cached data")
  inv <- solve(mcache)
  x$setInverse(inv)
  return(inv)
}
