## Caching the inverse of a matrix
## Matrix inversion is a costly computation
## So we cache the inverse of a matrix rather than compute it repeatedly

## This function will create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    
    ## Use of << operator to assign a value to an object in an environment
    ## that is different from the current environment.
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set =  set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function will compute the inverse of the matrix create by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed, 
## the cacheSolve function will retrieve the inverse from the cache,
## thus saving computation time.

cacheSolve <- function(x, ...) {
  invmat <- x$getInverse()
  if(!is.null(invmat)) {
    message("Displaying cached data")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setInverse(invmat)
  invmat
}
