## This functions are used to create a special object that contains a matrix
## and cache's its inverse matrix.

## This is a function that builds a list in which there are functions
## to set and get the value of the matrix and functions to set and get the inverse value

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y){
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_m <<- solve(x)
  getInverse <- function() inv_m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This is a function that calculates the the inverse matrix to the original one
## it has two parts. In the first part it checks whether the inverse matrix is 
## already calculated, in this case it takes it from the cache, or not, in this
## case it calculates the inverse matrix and set it in the cache using the seInverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getInverse()
  if (!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  mat1 <- x$get()
  inv_m <- solve(mat1, ...)
  x$setInverse(inv_m)
  inv_m
}

