## Put comments here that give an overall description of what your functions do
################################################################################################
# The functions "makeCacheMatrix" and "cacheSolve" reduce the processing time when calculating
# the inverse of a square matrix repetitively by caching the value of the inverse matrix
# for future use.
#
# Upon using these functions for the first time on a given matrix, the matrix is inversed and 
# stored in memory as matrix 'm'. If that same matrix is called to be inversed a subsequet time, 
# the value is returned from 'm' rather than being re-calculated. 
#
# The functions below are adapted from "makeVector" and "cachemean" (the examples from class).
# They are modified to cache the inverse of a matrix using the "solve" function; rather than
# caching the mean, which is what the original functions do. 
################################################################################################



## Write a short comment describing this function 
################################################################################################
# makeCacheMatrix takes a square matrix as in input, and returns a (list) vector of functions -
# set, get, setInverseMatrix, and getInverseMatrix which do the following:
#
# set: sets the value of the Matrix
# get: gets the value of the Matrix
# setInverseMatrix: sets the value of the inverse matrix
# getInverseMatrix: gets the value of the inverse matrix
#
# added note: the 'set' method sets the variable 'x' and 'm' in the parent environment 
# 'm' is used by cacheSolve to check if the inverse matrix has already been calculated
# 'x' is the matrix passed to cacheSolve to be inverted. (if it has not been inversed previously)
################################################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## Write a short comment describing this function
################################################################################################
# cacheSolve takes the vector returned from makeCacheMatrix and returns the inverse of the square 
# matrix that was entered into makeCacheMatrix EITHER by calculating it, OR by returning the value 'm' 
# from the cache (in cases where the inverse of a given matrix is being requested more than once)
#
# This is done by checking if 'm' is  Not NULL: if(!is.null(m))
# When 'm' IS null, it means the inverse matrix has not yet been calculated, so it gets calculated.
# On subsequent iterations, m <- x$getInverseMatrix() will set 'm' to the inverse matrix, making
# if(!is.null(m)) = TRUE, thus executing the lines: return 'm', rather than re-calculating 'm'.
################################################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
  
}
