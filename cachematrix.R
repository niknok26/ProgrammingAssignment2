##
## COURSERA R Programming: Programming Assignment #2
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(o_matrix = matrix()) {
  
  i_matrix <- NULL # Set the initial value of the inverse matrix to NULL
  
  # Sets a value for the matrix, and sets inverted matrix to NULL (matrix has changed)
  setMatrix <- function(z) {
    o_matrix <<- z
    i_matrix <<- NULL
  } 
  
  # Returns the current value of the original matrix
  getMatrix <- function() o_matrix
  
  # Sets the value of the inverse matrix
  setInverseMatrix <- function(z) i_matrix <<- z
  
  # Returns the value of the inverse matrix
  getInverseMatrix <- function() i_matrix
  
  # Returns the functions in a list
  list( setMatrix=setMatrix, getMatrix=getMatrix, 
        setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  # Retrieve current value of inverse matrix
  i_matrix <- x$getInverseMatrix()
  
  # Check if inverse matrix is null. Get cached inverse matrix if so.
  if (!is.null(i_matrix)) {
    message("Getting Cached Data")
    return(i_matrix)
  }
  
  # Get original matrix and compute inverse using solve()
  data <- x$getMatrix()
  i_matrix <- solve(data)
  
  # Set computed value of original matrix as cached inverse matrix
  x$setInverseMatrix(i_matrix)
  i_matrix
}
