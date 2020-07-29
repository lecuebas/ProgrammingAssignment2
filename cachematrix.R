# ProgramingAssignment2 29/07/2020 

# Function makeCacheMatrix takes a matrix and makes its "cachable" version, 
# so every time cacheSolve is called, it firstly checks if the result is already
# cached and computes the matrix inverse only if not.


# Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set: sets the new matrix to be cachable
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get: gets the currently stored matrix
  get <- function() x
  
  # setinverse: caches the inverse of the stored matrix
  # getinverse: retrieves the cached inverse of the matrix
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function () m
  list(set = set, get = get,
       setinverse = setinverse
       getinverse = getinverse)
  
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve retrieves the inverse from the cache.



cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)){
          message("getting cache data")
          return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  
  m
}
