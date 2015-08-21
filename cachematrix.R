## Caching the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the value of the matrix
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function () x
  ## set the value of the inverse of the matrix
  setsolve <- function (solve) m <<- solve
  ## get the value of the inverse of the matrix
  getsolve <- function () m
  ## return list
list (set = set, get = get,
      setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ##checks to see if the inverse has already been calculated
        ## return a matrix that is the inverse of 'x'
        m <- x$getsolve ()
        if(!is.null(m)) {
          message ("getting cached data")
          return (m)
        }
        ## calculates the data, if it doesn't exist in cache
        data <- x$get()
        m <- solve (data, ...)
        ## sets the value of the inverse in the cache via the setsolve function
        x$setsolve (m)
        m
}
