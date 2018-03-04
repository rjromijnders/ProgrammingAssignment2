## Create a matrix object and store its inverse in cache
## In this way the object remains in memory an subsequent access is accelerated

## Create an R object that stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y                    # assign input argument to "x" object in parent environment
    inverse_matrix <<- NULL    # reset the "inverse_matrix" object
  }
  get <- function() x
  set_inverse <- function(solve) inverse_matrix <<- solve
  get_inverse <- function() inverse_matrix
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Retrieve the inverse matrix from the input argument (an makeCacheMatrix object)
## If the inverse matrix is not yet cached, then calculate the inverse
cacheSolve <- function(x, ...) {
  inverse_matrix <- x$get_inverse()      # get the matrix object
  if(!is.null(inverse_matrix)) {         # if matrix object is not a NULL object
    message("getting cached data")       # retrieve the value from cache
    return(inverse_matrix)
  }
  data <- x$get()                        # get data from input argument
  inverse_matrix <- solve(data, ...)     # calculate the inverse of the matrix (call to "solve()" function)
  x$set_inverse(inverse_matrix)          # set the inverse of the matrix object
  inverse_matrix
}
