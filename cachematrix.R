## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix - Create a cache for inverse matrix  
#
# set - to set matrix and inverse matrix fields
# get - to get matrix fields
# getinverse - to get inverse matrix value
# setinverse - the set the inversematrix value
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# Given a cache matrix return cached inverse value or
# calculate the inverse and cache it. Returns the inverse
# matrix value from 'solve(m, ...)' function call or from
# cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
