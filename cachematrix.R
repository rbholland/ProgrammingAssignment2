## This function caches the inverse of the matrix, and loops through the matrix, without having to calculate the inverse more than once
makeCacheMatrix <- function(x = matrix()) {
  cacheInverse <- NULL
  set <- function (y) {
    x <<- y
    cacheInverse <<- NULL
  }
  
  get <- function() x
  setCachedInverse <- function(inverse) cacheInverse <<- inverse
  getCachedInverse <- function() cacheInverse
  
  list(set = set, get = get, setCachedInverse = setCachedInverse, getCachedInverse = getCachedInverse)
}


## Write a short comment describing this function
## Returns a matrix that is the inverse of input 'x'
cacheSolve <- function(x, ...) {
  cached <- x$getCachedInverse()
  if (! is.null(cached)) {
    return(cached)
  }
  m <- x$get()
  cached <- solve(m)
  x$setCachedInverse(cached)
  cached
}
