## These are two functions, makeCacheMatrix() and cacheSolve() that 1) cache the value of the inverse
## of an invertible matrix and 3) check if a value is cached; retrieving it if so or calculating it if not.

## NOTE: cacheSolve() expects to be passed input from makeCacheMatrix(). In other words, don't
## pass a matrix directly into cacheSolve() as it will throw an error. Instead, pass the matrix
## into makeCacheMatrix() before passing the result into cacheSolve().

## The first function, makeCacheMatrix creates a special "matrix", which contains a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse (solve)
## 4. get the value of the inverse (solve)

makeCacheMatrix <- function(x = matrix()) {

## 1. Setting value of matrix
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

## 2. Getting value of matrix
  get <- function() x
  
## 3. Setting value of inverse (solve)
  setsolve <- function(solve) m <<- solve
  
## 4. Getting value of inverse (solve)
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the mean of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse (solve) has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {

## Checking if inverse has already been calculated and if so, retrieving it from cache
    m <- x$getsolve()
      if(!is.null(m)) {
    message("getting cached data")
    return(m)
      }

## Calculating if not already calculated
  data <- x$get()
  m <- solve(data, ...)

## Setting the calculated data in cache
  x$setsolve(m)
  m
}
