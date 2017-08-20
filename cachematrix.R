## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## initialization of X with a default value
makeCacheMatrix <- function(x = matrix()) {
  ## initializes object inv
  inv <- NULL
  set <- function(y) {
    ## assigns the input argument to the x object in the parent environment
    x <<- y
    ## assign the value of NULL to the inv object in the parent environment
    inv <<- NULL
  }
  ## getter for x
  get <- function() x
  ## setter for the inverse matrix inv, gets value from the parent environment
  setinverse <- function(invert) inv <<- invert
  ## getter for inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## cachesolve takes matrix x as an input
cacheSolve <- function(x, ...) {
  ## assigns value from function getinverse for matrix
  inv <- x$getinverse()
  ## checks if inverse of matrix x is calculated
  if(!is.null(inv)) {
    # if there is already existing value in inv, it prints message and value
    message("getting cached data")
    return(inv)
  }
  ## if inv is null, it assigns matrix x to the data by calling a function get()
  data <- x$get()
  ## and calculates value of the inverse matrix
  inv <- solve(data)
  ## sets inverse by calling function setinverse()
  x$setinverse(inv)
  ## prints inv value
  inv
}
