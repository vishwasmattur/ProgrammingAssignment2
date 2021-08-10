## Functions that are used in Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse as NULL
  i <- NULL
  ##set the matrix
  set <- function(matrix) {
    mat <<- matrix
    i <<- NULL
  }
  ##return the matrix
  get <- function() {
    mat
  }
  ##set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ##return the inverse of the matrix
  getInverse <- function() {
    i
  }
  ## Return a list of methods in the function
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}





## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##`above. If the inverse has already been calculated (and the matrix has not changed),
## then`cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## return the inverse matrix of 'x'
  mat <- x$getInverse()
  ## if inverse is already set, return the inverse
  if(!is.null(mat)) {
    message("...Fetching the cached data...")
    return(mat)
  }
  ## get the matrix
  data <- x$get()
  ## calculating inverse
  mat <- solve(data)%*%data
  ##setting the inverse
  x$setInverse(mat)
  ## ## Return a matrix that is the inverse of 'x'
  mat
}
