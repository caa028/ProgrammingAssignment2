## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # initialize the "i" variable that will hold the inverse
  i <- NULL
  # setter function for the actual matrix
  set <- function(y) {
    x <<- y
    i <- NULL
  }
  # getter function for the actual matrix
  get <- function() x
  # setter function for the inverse matrix
  setinverse <- function(inv) i <<- inv
  # getter function for the inverse matrix
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  # use the getter to obtain the inverse
  i <- x$getinverse()
  # if the inverse was obtained successfully, return the inverse
  if (!is.null(i)) {
    message("geting cached data")
    return(i)
  }
  # looks like there was no inverse cached... getting actual matrix
  data <- x$get()
  # computing the inverse
  i <- solve(data, ...)
  # caching the computed inverse
  x$setinverse(i)
  # return the inverse
  return(i)
}
