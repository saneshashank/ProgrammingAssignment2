## Put comments here that give an overall description of what your
## functions to calculate inverse of a given matrix and store the result in cache

## The following fuction allows a given matrix to be stored in cache.
## Allows the inverse of give matrix to be set in cache variable and allows to get the cached value.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  setmatrix <- function(y) {
    x <<- y
    invx <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) invx <<- inverse
  getinverse <- function() invx
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function returns the inverse of a given matrix based on cached value. 
## The function checks if inverse has already been calculated, if already calculated then returns the value from cache,
## else calculates the inverse of given matrix and sets the cache value for that matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$getmatrix()
  invx <- solve(data, ...)
  x$setinverse(invx)
  invx
}