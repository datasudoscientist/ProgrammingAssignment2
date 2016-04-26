#	The pair of functions below compute and cache the inverse of a matrix.
#	This function stores the value of the matrix and its inverse, and deletes the inverse when a new matrix is defined.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_mat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(inverse) inv <<- inverse 
  get_inv <- function() inv 
  list(set_mat = set_mat, get_mat = get_mat, 
       set_inv = set_inv, get_inv = get_inv)
}

#	This function returns the inverse of the matrix if already computed, and calculates it otherwise.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
  }
  mat <- x$get_mat()
  inv <- solve(mat, ...)
  x$set_inv(inv)
  inv	
}
