## These two functions create a matrix that can cache its own inverse

## This function creates a special matrix that caches its inverse.
## it does so by creating a list of functions to get and set the matrix
## and to get and set the inverse.

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


## This functions calculates the inverse of a matrix
## in the special form mentioned above.
## it first checks to see if the matrix has already cached its inverse,
## else it calculates it anew and caches the value for future reference.

cacheSolve <- function(x, ...) {
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
