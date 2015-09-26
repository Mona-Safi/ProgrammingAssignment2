## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.
## The two functions 'makeCacheMatrix' and 'cacheSolve' below peform computing and caching the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to

## 1.  set the value of a matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix
## 4.  get the value of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
    invm <- NULL
    set <- function(y) {
      x <<- y
      invm <<- NULL
    }
    get <- function() x
    setinvmat <- function(invmatrix) invm <<- invmatrix
    getinvmat <- function() invm
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.


## The assumption here is that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invm <- x$getinvmat()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  ## Now compute the inverse of the matrix
  invm <- solve(data)
  x$setinvmat(invm)
  invm
  
}
