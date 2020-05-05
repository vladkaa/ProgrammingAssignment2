## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse. The idea behind cache is to save computing power.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) inv <<- solve(x) #here calculates inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrx <- x$get()
  inv <- solve(matrx, ...)
  x$setInverse(inv)
  inv
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
}

#Example to use function
matrx <- makeCacheMatrix()

## set the matrix value
matrx$set(matrix(data = (1:10), nrow = 5, ncol = 2))

## Check that we stored it correctly
matrx$get()
