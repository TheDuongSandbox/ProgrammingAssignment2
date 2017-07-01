## Matrix inversion is expensive, so it's important to repeat the computation unless absolutely necessary
## The following functions work together to make that entire process transparent to the users

## This function returns a wrapper around an ordinary matrix that makes caching its inversion easy
## And make sure the inversion is set to NULL everytime the matrix's value changed.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) inverse <<- solve
  getSolve <- function() inverse
  
  list(
    set = set,
    get = get,
    setSolve = setSolve,
    getSolve = getSolve
  )
}


## This function makes sure the inversion is only compute once unless the matrix changes and its inversion is clear
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getSolve()
  if (!is.null(inverse)) {
    message("Getting cache data")
    return(inverse)
  }
  
  inverse <- solve(x$get(), ...)
  x$setSolve(inverse)
  inverse
}
