## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.  
## Assumption: Only square matrices are passed in as formal arguments.

## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to
## set the value of the matrix (mutator)
## get the value of the matrix (accessor)
## set the value of the inverted matrix (mutator)
## get the value of the inverted matrix (accessor)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## The following function calculates the inverted matrix of the special "matrix" created
## with the above function. However, it first checks to see if the inverted matrix has
## already been computed. If so, it gets the inverted matrix from the cache and skips the
## computation. Otherwise, it computes the inverted matrix and updates the cache with the
## newly computed value via the setSolve function.
## 
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
