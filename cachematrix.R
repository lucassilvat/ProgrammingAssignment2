## Functions makeCacheMatrix and cacheSolve work together in order to avoid
## repeating inverting matrix calculations by looking for cached data

## makeCacheMatrix creates a special "matrix" that has four functions in it.
## 1. set function -> changes the original matrix to the input matrix and resets its inverse
## 2. get function -> returns the stored matrix
## 3. setinv function -> changes the inverse matrix to the input matrix
## 4. getinv function -> returns the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y = matrix()){
    x<<-y
    i<<-NULL
  }
  
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve function looks for stored inverse matrix of 'x' and, if it was
## never calculated, does it and stores it. Then, returns the inverse matrix of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}
