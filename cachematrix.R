## The functions contained in cachematrix.R are for creating a matrix with
## where the results of inverse calculation are cached.
## This code has been adapted from the makeVector()/cacheMean() solution provided as an 
## example and then modified to fit the specific requirements for this project.
## 

## The makeCacheMatrix() function takes a matrix as a parameter.
## The passed matrix is 'wrapped' and returned as a 'cacheable' matrix
## 
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i 
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve processes requests for inverse calculation
## If the inverse has not yet been calculated, it is calculated with a call to solve(),
## and the results are then cached.  Subsequent calls to cacheSolve will return the cached
## result.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}