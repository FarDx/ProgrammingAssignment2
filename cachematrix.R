## These functions are made to handle a matrix easier and
## make operations on it. It can cache the matrix in the main
## memory, saving computational time if it's already made.
## Author: Rafael Palomino
## Date: 21-07-2014

## This function constructs the structure necessary to apply
## operations.
## Input:  an R object of type matrix.
## Output: a list containing the operations to get and set
##         the matrix and his inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function constructs the structure necessary to apply
## operations.
## Input:  an R object of type makeCacheMatrix. (an 'special' 
##         list returned for makeCacheMatrix)
## Output: A matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
