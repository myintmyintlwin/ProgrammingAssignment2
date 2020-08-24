## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This is the computing the inverse of a square matrix can be done 
##with the solve function in R

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix<- NULL
  set <- function(y) {
    x <<- y
    inv_matrix<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_matrix <<- inverse
  getInverse <- function() inv_matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return inverted matrix of x
  
  inv_matrix <- x$getInverse()
  if (!is.null(inv_matrix)) {
    message("getting cached matrix")
    return(inv_matrix)
  }
  mat <- x$get()
  inv_matrix <- solve(mat, ...)
  x$setInverse(inv_matrix)
  inv_matrix
  
}
