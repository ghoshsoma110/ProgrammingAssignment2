## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a
## matrix rather than compute it repeatedly. 
## In this assignment I am writing a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){                           # set the value of the vector
    x <<- y
    j <<- NULL
  }
  get <- function()x                            # get the value of the vector
  setInverse <- function(inverse) j <<- inverse # set the value of the inverse
  getInverse <- function() j                    # get the value of the inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

## Testing the inverse matrix

m <- matrix(rnorm(25),5,5)
