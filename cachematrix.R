## Purpose: to cache the inverse of a matrix
## Matrix inversion can be costly in terms of computation and there can be some
## benefit to utilising the cache rather than computing it repeatedly.
## The below functions are used to create a special matrix object and cache
## its inverse...

## makeCacheMatrix is the first function, this will create a matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The second function cacheSolve computes the inverse of the matrix object
## defined by makeCacheMatrix. If the inverse has already been created and 
## the matrix has not changed, the result will be retreived from the cache!

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message ("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}