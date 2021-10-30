## Put comments here that give an overall description of what your
## functions do
##The first function creates a matrix to cache its inverse. the second function compute 
##the inverse of the matrix. If already cached, it returns the cached result.


## Write a short comment describing this function
## It creates a matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setInve <- function(inverse) inve <<- inverse
  getInve <- function() inve
  list(set = set, get = get,
       setInve = setInve,
       getInve = getInve)
}


## Write a short comment describing this function
## Compute the inverse of the matrix. If already cached, it returns the cached result.

cacheSolve <- function(x, ...) {
  inve <- x$getInve()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setInve(inve)
  inve
  }
