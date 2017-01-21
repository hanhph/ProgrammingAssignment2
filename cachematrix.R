### This function creates a special "matrix" object that can cache its inverse. It returns a list of 4 functions to serve this purpose
makeCacheMatrix <- function(x=matrix()) {
  #Initialize the inverse of the matrix by setting up a null matrix
  inverse_matrix <- NULL
  # Function "set" to set the value of the matrix and its inverse in case the matrix has changed (or be created for the first time)
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  # Function "get" to obtain the new/changed matrix
  get <- function() x
  # Function "set_inverse" to calculate the new inverse
  set_inverse <- function(solve) inverse_matrix <<- solve
  # Function "get_inverse" to retrieve the inverse from cache
  get_inverse <- function() inverse_matrix
  # Return the list of functions 
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
### If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
### retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Acess the cache to get the inverse matrix and store this in inverse_matrix
  inverse_matrix <- x$get_inverse()
  # If such inverse exists (not NULL), then print this value to the console
  if(!is.null(inverse_matrix)) {
    message("getting cached data for matrix inverse")
    return(inverse_matrix)
  }
  # Otherwivse, use the rest of the functions in makeCacheMatix to store and calculate 
  # the inverse for the new matrix, then print the result to the console
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$set_inverse(inverse_matrix)
  inverse_matrix
}
