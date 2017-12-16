## I cached the inverse of a matrix. Overall, these two functions are able 
## to cache time-consuming computations.Caching the inverse of a matrix 
## rather than compute it repeatedly was completed here by writing the 
## following pair of functions. 

## The "makeCacheMatrix" function creates a matrix object that can cache the
## inverse of itself. 

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setinv <- function(solve)a <<- solve
  getinv <- function()a
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The "cacheSolve" function computes the inverse of the matrix object via 
## the "makeCacheMatrix" function. If the inverse has been calculated (and 
## the matrix has not changed), then it retreives the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
  a <- x$getinv()
  if (!is.null(a)){
    message ("get cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinv(a)
  a
}