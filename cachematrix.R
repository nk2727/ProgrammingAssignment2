
##This function creates a special "matrix" object,x, that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    #
    # return x
    #
    x
  }
  
  setMatrix <- function(solve) {
    m <<- solve
  }
  
  getMatrix <- function() {
    m
  }
  
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
  
}


## Write a short comment describing this function
#
# This funnction returns a matrix that is the inverse of input 'x'
# by calling the functions in the makeCacheMatrix above
# It first tries to see if the inverse of input x exists
# if it does, indicate so by the message and returns the inverse
# Otherwise, the inverse of x is calculated, cached and returned
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    
    if (!is.null(m)) { 
      message("Getting cached data")
      return(m)
    }
    
    matrix <- x$get()
    
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}

