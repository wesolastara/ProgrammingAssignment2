## Programming Assignment 2 wrote by Maciej Gielnik for the Coursera
## R programming course

## This function prepares values for the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    ## a flag for checking if inversion was calculated is set to NULL
  setmatrix <- function(y) {   ## setting matrix
    x <<- y
    m <<- NULL ## a flag for checking if inversion was calculated is set to NULL
  }
  getmatrix <- function() x    ## getting matrix 
  setsolve <- function(solve) m <<- solve   ## setting inversion of a matrix 
  getsolve <- function() m                  ## getting inversion of a matrix
  list(setmatrix = setmatrix, getmatrix = getmatrix, ## making a list with values
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function calculates the inversion of a matrix ans cache result

cacheSolve <- function(x, ...) {
  m <- x$getsolve()    ## loads m from makeCacheMatrix
  if(!is.null(m)) {                    ## checks if the inversion was calculated 
    message("getting cached data")     ## if it was returns cached data from memory
    return(m)
  }
  data <- x$getmatrix()                ## gets data for calculation
  m <- solve(data, ...)                ## calculates the inversion of a matrix
  x$setsolve(m)                        ## fills a list with a calculated inversion of a matrix
  m                                    ## result
}


