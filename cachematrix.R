## Coursera course: R programming
## Made by Jasmijn Kol

##Cashing the inverse of a matrix by creating two functions.
## Function 1: caches the inverse of a matrix object
## Function 2: Computes the inverse returned by Function 1. If inverse has been calculated already
##             the function retrieves the inverse from the cache.

## Function 1:
makeCacheMatrix <- function(x = matrix()) {
  ## Initializing inverse as an object
  inverse <- NULL
  
  ##set matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## get matrix
  get <- function() x
  
  ## set inverse of the matrix
  setm <- function(solving) inverse <<- solving
  
  ## get inverse of the matrix
  getm <- function() inverse
  
  list(set=set, get=get, setm = setm, getm=getm)
}


## Function 2:
cacheSolve <- function(x, ...) {
  
  ## return inverse of matrix (assuming that matrix is invertible)
  inverse <- x$getm()
  
  ## returning inverse cached data
  if(!is.null(inverse)) {
    message("receiving cached data")
    return(inverse)
  }
  
  ## get matrix
  data <- x$get()
  
  ## calculation of the inverse of the matrix
  inverse <- solve(data)
  
  x$setm(inverse)
  
  ## return matrix
  inverse
}
