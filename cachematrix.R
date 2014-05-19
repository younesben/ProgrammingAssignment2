## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initializing the variable m 
  m <- NULL
  # setting x and m beyond the scope of the functions i.e. caching
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # getting x
  get <- function() x
  # setting m as the inverse beyond th scope of the functions : i.e. caching
  setInverse <- function(solve) m <<- solve
  # getting the inverse 
  getInverse <- function() m
  # returning a list of functions that will be used to store and retrieve the matrix and its inverse from cache.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # getting the inverse of x
  m <- x$getInverse()
  if(!is.null(m)) {
    # if x$setInverse() has already been called, the inverse is retrieved from cache by x$getInverse() and returned.
    message("getting cached data")
    return(m)
  }
  # else the inverse is computed, cached by x$setInverse(), then returned.
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
