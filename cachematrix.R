## Programming Assignment 2: Lexical Scoping
## There are a pair of functions that can calulate and cache the inverse of a matrix.

## Creating a special "matrix" object that can cache its inverse.
## It allows getting and setting matrix and its inverse via caching.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
  }



## This function computes the inverse of the special "matrix" object.
## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.
## Otherwise, it calculates the invers of a matrix and sets it in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    ## check if the inverse has already been calculated
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    ## Return a matrix that is the inverse of 'x'
    return (m)
}
