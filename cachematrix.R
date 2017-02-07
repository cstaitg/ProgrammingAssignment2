## This code is for the assignment in week 3 of the R Programming course
## offered by Coursera. 
## 
## It contains two functions:
##
## 1. makeCacheMatrix: This function creates a special "matrix" object that can
##    cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been 
##    calculated (and the matrix has not changed), then cacheSolve should 
##    retrieve the inverse from the cache.


## makeCacheMatrix- create matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
     InvMat <- NULL
     set <- function(y) {
          x <<- y
          InvMat <<- NULL
     }
     get <- function() x
     setInv <- function(solve) InvMat <<- solve
     getInv <- function() InvMat
     list(set = set,
          get = get,
          setInv = setInv,
          getInv = getInv)
} 

## Return a matrix that is the inverse of 'x' 
cacheSolve <- function(x, ...) { 
     InvMat <- x$getInv()
     if(!is.null(InvMat)) {
          message("getting cached inverse")
          return(InvMat)
     }
     data <- x$get()
     InvMat <- solve(data, ...)
     x$setInv(InvMat)
     InvMat
}
