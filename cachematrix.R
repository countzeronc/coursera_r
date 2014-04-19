setwd("C:/Users/Jack/Documents/GitHub/coursera_r")

## The first function, makeVector creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize inverted matrix
  m <- NULL
    
  # define four objects
  set <- function(y) {
         x <<- y
         m <<- NULL
  }  
  get <- function() x
  setInvMatrix <- function(solve) m <<- solve
  getInvMatrix <- function() m

  # create the list object
  list(set = set, 
       get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## The following function calculates the inverse of the special "matrix" created with makeCacheMatrix. 
## However, it first checks to see if the inverted matrix has already been calculated. If so, it gets the 
## inverted matrix from the cache and skips the computation. Otherwise, it solves the matrix 
## and sets the value of the inverted matrix in the cache via the setInvMatrix function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' as m
  m <- x$getInvMatrix()
  
  # if inverted matrix exists, return that
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise invert matrix and return;
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}
