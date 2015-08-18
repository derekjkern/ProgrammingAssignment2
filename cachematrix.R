## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#
# This function will define the matrix so that it has 
# caching behaviors.
#
makeCacheMatrix <- function( x = matrix() ) {
  #
  # Initialize the inverse to NULL so it is not initially cached
  #
  inv <- NULL
  
  #
  # When we set the matrix with new data, then we want to clear the inverse 
  # as it must be recalculated
  #
  set <- function( p_mat ) {
    x <<- p_mat
    inv <<- NULL
  }
  get <- function() x
  
  #
  # Simple functions for setting an retrieving the inverse
  #
  setinv <- function( p_inv ) inv <<- p_inv
  getinv <- function() inv
  
  list( 
    set = set, 
    get = get,
    setinv = setinv,
    getinv = getinv
  )  
}


## Write a short comment describing this function

#
# This function will calculate the inverse of a matrix defined
# using makeCacheMatrix(). If the inverse has already been
# calculated for the passed matrix (x), then it will simply 
# return this result.
#
cacheSolve <- function( x, ... ) {
  ## Return a matrix that is the inverse of 'x'
  
  #
  # Get the current inverse set on the passed matrix x.
  #
  # If it is already set, i.e. cached, then simply return this
  # as the result.
  #
  inv <- x$getinv()
  if( !is.null( inv ) ) {
    message("getting cached data")
    
    return( inv )
  }
  
  #
  # If we are here, then the result was not cached on the matrix (x).
  #
  # Get the raw matrix from the passed matrix (x) and calculate its inverse
  # with the solve function.
  #
  # Once the inverse has been calculated, cache the result on the passed
  # matrix.
  #
  data <- x$get()
  inv <- solve( data, ... )
  x$setinv( inv )
  
  inv  
}
