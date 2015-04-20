## Reduce computation time by storing matrix inverse in memory cache
## Check memory for matrix inverse and return inverse if found
## If not found, calculate inverse with solve() and store inverse in cache

## Creates a special matrix object that can cache a matrix inverse
## Contains functions to set and get value of a matrix and set/get value of the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL(())
  }
  get <- function() x
  setInverse <- function(inv) Inv <<- inv
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates the inverse of a matrix returned by makeCacheMatrix
## first checking to see if the inverse is already, calculated and stored in memory, 
## then calculating the inverse if not yet stored; if stored, cacheSolve returns the
## stored matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
}
