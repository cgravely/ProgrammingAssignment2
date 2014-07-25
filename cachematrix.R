## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "vector". 
## The results of makeCacheMatrix are used as input to the cacheSolve function.

makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function caclulates the inverse of the special "vector"
## created by the makeCacheMatrix function.  The results are cached and
## used in subsequent function calls so as to avoid recalculating.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
