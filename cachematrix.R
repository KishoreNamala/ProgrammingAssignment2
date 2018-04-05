## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## makeCacheMatrix - returns a special vector which is really a list containing a function to
##      set the value of the input matrix against which we do the computation
##      get the value of the matrix set in the input using the set method
##      set the value of the inverse 
##      get the value of the inverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setInverse <- function(inverse)
    m <<- inverse
  getInverse <- function()
    m
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## cacheSolve returns the inverse of the matrix in the input vector
## It checks if the input vector already has a computed inverse stored in it and returns the same if it exists
## otherwise, computes it and sets it on the input vector.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
