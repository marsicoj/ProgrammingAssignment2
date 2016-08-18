## These functions accept values for a matrix, create its inverse, and store the 
## inverse in a "cache"

## This function accepts a series of values for a matrix that can be inversed
## in a cache.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(get = get, setinv = setinv, getinv = getinv)
}

## This function returns an inverse and returns an inverse from the cache 
## if it is stored.

cacheSolve <- function(x, ...) {

  i <- x$getinv()
  if(!is.null(i)) {
    print("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  print("Not retrieving cached data")
  x$setinv(i)
  i
  
  }
