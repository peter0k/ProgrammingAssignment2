#Matrix inversion is usually a costly computation and
#there may be some benefit to caching the inverse of
#a matrix rather than computing it repeatedly.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve's argument is the function list returned by
## makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cachesolve returns
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## enter cache, return inv if m is not NULL
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## testing
install.packages("microbenchmark")
library(microbenchmark)

X = matrix(c(2,2,3,2),2,2)
x <- makeCacheMatrix(X)
cacheSolve(x)
solve(X)

microbenchmark(
  cacheSolve(x),
  solve(X)
)

