## Put comments here that give an overall description of what your
## functions do
## The following functions calculate the inverse of a matrix and saves it
## to the cache . If the same operation is attempted again and there is a cache hit
## data is retured from   cache rather repeating the calculation.



## Write a short comment describing this function
## This function creates a "matrix" object.
## It also has the getter and setter for the matrix
## Additionally getter and setters for the invesrse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  ## create a matrix object x that can cache its inverse
  
  ## define the cache m
  m <- NULL
  set <- function(y) {
    x <<- y   ## Input matrix y to the variable x in the parent environment
    m <<- NULL ## Empty Cache
  }
  
  get <- function() x ## return the matrix x
  
  setinverse <- function(inverse) m <<- inverse ## Set the cache m to inverse of x
  
  getinverse <- function() m ## Return the cached inverse of x
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## Calculates the inverse of the  "matrix" created by makeCacheMatrix()
## First checks to see if the inverse is present.
## If it exists returns cache value without further processing
## If cached value is not present matrix inverse is calculated and returned.
## This a;so refreshes the cache by setting with newly calculated value.

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("Cache Hit..")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
