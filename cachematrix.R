## Functions makeCacheMatrix and cacheSolve allow for the inversion
## of a square matrix in such a way, that the inverse is calculated only
## if a new matrix is provided. The inverse is taken from the cache if
## it has been previously calculated for the same matrix.


## function makeCacheMatrix takes a matrix as an input
## and returns a list containing functions to 
## get and set the matrix and get and set its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## function cacheSolve takes a list of functions as an input
## and returns the matrix inverse (from the cache, if possible).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
