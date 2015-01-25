## The following functions are used to calculate matrix inverses for use in time critical
## computations. The inverse is cached so that it is not recalculated repeatedly.

## Creates a cacheMatrix object that can store a matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the matrix and caches it. If the inverse has been previously calculated, this function returns the value from the cache rather
## than calculating it again.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}