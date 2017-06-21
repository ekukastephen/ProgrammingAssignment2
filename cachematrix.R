## Put comments here that give an overall description of what your
## functions do
## These functions introduce a special matrix object which caches its inverse.

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    # Set the value
    m <<- NULL # Clear the cache
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
    
}


## Write a short comment describing this function
## This function returns inverse of matrix x

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m 
  
}
