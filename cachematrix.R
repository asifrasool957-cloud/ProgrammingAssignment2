## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y    # Assign new matrix to x in parent environment
    inv <<- NULL  # Reset inverse because the matrix changed
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of the four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, it retrieves it from the cache
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()  # Check if inverse is already cached
  
  if(!is.null(inv)) {
    message("getting cached data")  # If cached, return it
    return(inv)
  }
  
  # If not cached, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse using solve()
  
  x$setInverse(inv)  # Cache the inverse for future use
  
  inv  # Return the inverse
}

# Example usage:
# my_matrix <- matrix(c(2, 0, 0, 2), nrow=2, ncol=2)
# cachedMatrix <- makeCacheMatrix(my_matrix)
# cacheSolve(cachedMatrix)  # First time calculates
# cacheSolve(cachedMatrix)  # Second time retrieves cached inverse
