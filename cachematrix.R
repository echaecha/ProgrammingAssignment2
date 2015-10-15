## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions as follows:
## 1. Sets the value of the matrix (and NULLs any inverse value)
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse (and leaves the matrix alone)
## 4. Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes a list created by makeCacheMatrix to do the following:
## 1. Check if inverse value is already stored; if so, return this value.
## 2. If not, retrieve the matrix instead, and calculate its inverse, to store it in the list.
## 3. Also return the value of the inverse that was calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("retrieving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
