## The functions in this file implement a caching functionality
## for calcuating inverse of a matrix. Inverse calculation is
## computationally intensive and repeated calculations can result
## in slowing down of the program execution. 
## This implementatin avoids repeated inverse calculation of a matrix
## by storing a copy of the inverse computed once and reused later
## 

## "makeCacheMatrix" function is used to create a cache object
## that stores the original matrix passed along with its inverse
## The inverse is initialized to NULL when the cache is created.
##
## The returned cache object provides the following functions:
## "get" - used to retrieve the original matrix stored in the cache
## "set" - used to store a new matrix into the cache. This also sets
##         the inverese value to NULL
## "getinv" - returns the inverse of the stored matrix. Returns the
##            inverse from the cache if available otherwise computes it
## "setinv" - stores the inverse in the cache object for later use
## 

makeCacheMatrix <- function(x = matrix()) {
  ## Set the inverse value to NULL
  inv <- NULL
  set <- function(y) {
    ## Set the new matrix in the cache and set inv to NULL
    x <<- y
    inv <<- NULL
  }
  # Get returns the stored matrix
  get <- function() x
  
  # setinv sets the computed inverse in the cache
  setinv <- function(inverse) inv <<- inverse
  
  #getinv returns the stored inverse value
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## "cacheSolve" function returns the inverse of the matrix
## stored in the cache. It checks if the inverse has already
## been computed and available in the cache. If the inverse
## is available (not NULL), it is returned from the cache.
## If inverse is NULL, it is computed and stored for future use
## 


cacheSolve <- function(x, ...) {
  # Get the inverse from the cache
  inv <- x$getinv()
  if(!is.null(inv)) {
    # A non-null value in inv, means inverse is available
    # in the cache. Just return it
    message("getting cached data")
    return(inv)
  }
  # inv is NULL. This is the first time cacheSolve is called
  # on this cache. Compute the inverse and store it before
  # returning the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
