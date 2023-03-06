## Programming Assignment 2: Lexical Scoping
## Create a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) { ## set the value of the matrix
    x <<- y
    s <<- NULL
  }
  get <- function() x ## get the value of the matrix
  setsolve <- function(solve) s <<- solve ## set the value of the inverse
  getsolve <- function() s  ## get the value of the inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) ## caching...
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)){ ## checks if the inverse has already been calculated
    message("getting cached data")
    return(s) ## retrieves data
  }
  data<-x$get() ## gets data from x
  s<-solve(data,...) ## computes the inverse
  x$setsolve(s) ## sets the inverse
  s ## Returns the inverse matrix
}