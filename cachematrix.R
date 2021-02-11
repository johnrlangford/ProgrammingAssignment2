## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix returns a cacheMatrix object
## with methods to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  x.inv <- NULL
  set.matrix <- function(y) {
    x <<- y
    x.inv <<- NULL
  }
  get.matrix <- function() x
  
  get.inverse <- function() x.inv
  
  set.inverse <- function(y) x.inv <- y
  
  list(set = set.matrix, get = get.matrix,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
  }



## Write a short comment describing this function
## cacheSolve takes a cacheMatrix object and returns its inverse
## if the inverse is already cached by cacheMatrix object, it is
## returned immediately. if it isn't cached, it is calculated,
## cached, and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x.inv <- x$get.inverse()
  if (!is.null(x.inv)) {
    message("getting cached data")
    return(x.inv)
  }
  x.mat <- x$get()
  x.inv <- solve(x.mat)
  x$set.inverse(x.inv)
  x.inv
}
