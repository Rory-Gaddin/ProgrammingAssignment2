## ----------------------------------------------------------------------------
##  cacheMatrix.R
##
##  Author  : Rory Gaddin
##  Date    : 2015-10-23
## ----------------------------------------------------------------------------
##  Allows for the creation of a new matrix object in which the inverse of the
##  matrix is cached when requested.  This is intended to spped computation
##  where the inverse of the matrix is likely to be frequently required by
##  numerous functions.
## ----------------------------------------------------------------------------

## Creates an instance of the matrix object with functions for returning and 
## setting the cacheable inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}
## ----------------------------------------------------------------------------

## Returns the inverse of the matrix for an object which was created using
## makeCacheMatrix.  The cache is populated the first time the function is 
## called.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()  # Attempt to read from the cache
    
    if(!is.null(inverse)) {
        message("Inverse found in cache!")
        return(inverse)
    }
    
    # Nothing in the cache, so we need to compute and store the inverse..
    mtx <- x$getMatrix()
    inverse <- solve(mtx)
    x$setInverse(inverse) # store in cache for next call to cacheSolve.
    
    inverse
}
## ----------------------------------------------------------------------------
