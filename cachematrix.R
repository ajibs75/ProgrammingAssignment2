
## This function can cache the inverse of an object by creating a special matrix of the object

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() iv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


##This function computes the inverse of the special "matrix" that is created by 
## makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$getInverse()
  if (!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  mat <- x$get()
  iv <- solve(mat, ...)
  x$setInverse(iv)
  iv
}
