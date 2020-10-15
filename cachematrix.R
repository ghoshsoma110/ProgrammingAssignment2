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

## Testing the inverse matrix ## Here rnorm(25),5,5 is a square invertible matrix. I am checking whether "m" is returning its inverse matrix or not.


m <- matrix(rnorm(25),5,5)
> m
           [,1]       [,2]        [,3]       [,4]       [,5]
[1,]  1.1655390 -2.1396614  1.44268521 -0.8990568  0.4417588
[2,] -0.2545757 -0.6606523  2.43053106 -1.2156187 -0.5205668
[3,] -1.1488525 -0.1761439  0.63408036  0.3722968 -0.2701398
[4,]  1.1983728  1.5789437  0.94422631  0.8273956 -0.8261121
[5,] -0.2485594  0.6493679 -0.05196703  0.6531061 -1.8468304
