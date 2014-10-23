
# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. 
# It takes the form of a list of four functions:
# get returns the matrix
# set sets the matrix and clears the cached inverse
# setinverse sets the cached inverse
# getinverse returns the cached inverse or null if unset
makeCacheMatrix <- function(x = matrix()) {

  # Start out with null inverse (haven't calculated it yet)
  matinverse <- NULL
  
  # Set function sets the matrix and clears the old, now-invalid inverse
  set <- function(newmat) {
    mat <<- newmat
    matinverse <<- NULL
  }
  
  # Get returns the matrix
  get <- function() x
 
  # Setinverse sets the newly calculated inverse for the matrix
  setinverse <- function(mean) matinverse <<- mean
  
  # Getinverse returns the cached matrix mean, or null if not yet set
  getinverse <- function() matinverse
  
  # Return a list of the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
#This function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above. If the inverse has already been calculated (and the
#matrix has not changed), then the cachesolve should retrieve the inverse from
#the cache.
cacheSolve <- function(x, ...) {
  
  # If we have a cached matrix inverse, return that
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # If no cached value, calculate the inverse
  mat <- x$get()
  inverse <- solve(mat) %*% mat
  
  # Cache the newly calculated inverse in the matrix object for later retrieval
  x$setinverse(inverse)
  
  # Return the inverse
  inverse
}