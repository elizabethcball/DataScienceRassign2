## These functions create a special object that stores a numeric matrix and  
## cache's its inverse. The <<- operator is used to assign a value to an 
## object in an environment that is different from the current environment

## The function makeCacheMatrix returns a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. However, it first checks 
## to see if the inverse has already been calculated. If so, it gets the  
## inverse from the cache (global scope) and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the verse in  
## the global variable m via the setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
