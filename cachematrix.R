## Put comments here that give an overall description of what your
## functions do

## The function sets the value of a matrix, then gets the value of the matrix, sets the value of the inverse and
## finally, it gets the value of the inverse. The double arrow assignment operator "<<-" allows us to make an
## assignment in the enclosing environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix we have created in the previous function. In case the value
## of the matrix is different from "NULL" it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}