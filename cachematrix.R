## Taking the inverse of a matrix may be a slow operation.
## If the matrix is not changing, it may make sense to cache the value
## of the inverse so that when we need it again, it can be looked up
## in the cache rather than recomputed


## makeCacheMatrix creates a special "matrix",
# which is really a list containing functions to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  cached <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    cached <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverse <- function(inverse) cached <<- inverse
  
  getInverse <- function() cached
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## the function cacheSolve calculates the inverse of the special "matrix"
# created with the above function. However, it first checks
# to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets
# the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cached <- x$getInverse()
  
  if(!is.null(cached)) {
    message("getting cached data")
    return(cached)
  }
  
  m <- x$getMatrix()
  inverse <- solve(m, ...)
  
  x$setInverse(inverse)
  
  inverse
}
