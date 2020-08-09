## A pair of functions that cache the inverse of a matrix
## This function creates a special "matrix" obuect that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  u <- NULL
  set <- function(y){
    x <<- y
    u <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) u <<- inverse
  getInverse <- function() u 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  u <- x$getInverse()
  if(!is.null(u)){
    message("getting cached data")
    return(u)
  }
  mat <- x$get()
  u <- solve(mat,...)
  x$setInverse(u)
  u
}
