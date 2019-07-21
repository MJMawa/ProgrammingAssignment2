## Functions for caching the inverse of a matrix

## Function returning a list of functions for setting/retrieving a matrix
## and Setting and retrieving the inverse of a matrix from the global
## environment

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set_matrix <- function(my_matrix) {
    x <<- my_matrix
    matrix_inv <<- NULL
  }
  get_matrix <- function() x
  setinverse_matrix <- function(inverse) matrix_inv <<- inverse
  getinverse_matrix <- function() matrix_inv
  list(set_matrix = set_matrix,
       setinverse_matrix = setinverse_matrix,
       get_matrix = get_matrix,
       getinverse_matrix = getinverse_matrix)

}


## Get the inverse of matrix supplied as argument
## from the global environment if it has been 
## computed or else compute and return the inverse
## of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inv <- x$getinverse_matrix()
  if (!is.null(matrix_inv)) {
    message("getting cached data")
    return(matrix_inv)
  }
  my_matrix <- x$get_matrix()
  matrix_inv <- solve(my_matrix, ...)
  x$setinverse_matrix(matrix_inv)
  matrix_inv
}
