
  ## [Put comments here that describe what your functions do]
  
  makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
      x <<- y
      inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inv) inv_x <<-inv
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}