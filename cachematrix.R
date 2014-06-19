## These functions will allow matrix inversion computations to be cached. When done once, the computation
## will not be required when using this special "matrix".

## makeCacheMatrix creates a special "matrix" or list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
## NOTE: ASSUMES MATRIX IS SQUARE AND INVERTIBLE

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of a matrix, after checking whether the inverse has already been calculated
## NOTE: ASSUMES MATRIX IS SQUARE AND INVERTIBLE

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached invert matrix")
    return(inv)
  }
  data <- x$get()
  inv <- getinverse(data, ...)
  x$setinverse(inv)
  inv
}