## This is a solution to the programming assignment #2 /
## of the R Programming course on Coursera.

## This function creates a matrix objects capable of caching and retrieving its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) inv <<- inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
#Below is just an example
makeCacheMatrix(x = matrix(data = c(1:49), nrow = 7, ncol = 7))

## This function calculates or retrieves (from cache) /
## the inverse of a matrix x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#Below is an example to check if this fuction works
cacheSolve(x = matrix(data = c(1:49), nrow = 7, ncol = 7))