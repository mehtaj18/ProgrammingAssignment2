## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## set the matrix
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  
  ## get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## get the inverse of the matrix
  getInverse <- function() {
    
    inv
  }
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  x <- x$getInverse()
  
  ## return the inverse if its already set
  if( !is.null(x) ) {
    message("getting cached data")
    return(x)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  x <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(x)
  
  ## Return the matrix
  x
  
}
