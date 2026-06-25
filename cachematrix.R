## This program features two functions (makeCacheMatrix and cacheSolve) that optimize matrix inversion in R using lexical scoping and the <<- operator.
## By caching the inverse of a matrix, it eliminates the need for repeated, computationally expensive recalculations.

## Creates a special "matrix" object that stores both the original matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the matrix inverse if the cache is empty; otherwise, it retrieves the pre-calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
}

## Creating an example to test the functions----
# Creating a simple 2x2 invertible matrix
test_matrix <- matrix(c(1, 2, 3, 4), 2, 2)

# Testing my functions
my_cached_matrix <- makeCacheMatrix(test_matrix)

cacheSolve(my_cached_matrix) # Computing and returning the inverse
cacheSolve(my_cached_matrix) # Says "getting cached data" and return it

