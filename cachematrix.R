## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## I've mostly used example functions for a neab of the vector, will need more times to understand scoping completly
makeCacheMatrix <- function(x = matrix()) {
  
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_m <<- inverse
  getinv <- function() inv_m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
cacheSolve <- function(x, ...) {
  inv_m <- x$getinv()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(inv_m)
  m
}


##solve(A)   Inverse of A where A is a square matrix.
##inv(A)   Moore-Penrose Generalized Inverse of A.
##ginv(A) requires loading the MASS package.