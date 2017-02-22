##Name: Caching the Inverse of a Matrix
##The functions below are created to calculate, store matrixs,
## and cache their reverses

##the first one is to cache reverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## the second function is to calculate the inverse of matrix
## in the function above
## when it comes to the matrix that was already computed before
## the function is able to retrieve the inverse directly from the
## cache that has been stored
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mtx <- x$get()
  i <- solve(mtx, ...)
  x$setinverse(i)
  i
}
