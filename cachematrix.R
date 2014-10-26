## This script defines two wrappers for dealing with "special matrix".
## In this example, we cache the inverse of this matrix for further usage.

## This function provides an interface for dealing with the "special matrix".
## It has interfaces like 
##     setting the matrix
##     getting the matrix
##     setting the inverse of matrix for caching
##     getting the cached inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    getMatrix <- function() {
        x
    }
    
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    getInverse <- function() {
        inv
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}

## This function is a wrapper for finding inverse of "special matrix".
## If the inverse of "special matrix" is already computed, get the cached
## value through its interface. Else, newly compute the inverse and store
## it via its interface.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("getting inverse from cache")
        return (inv)
    }
    
    mat <- x$getMatrix()
    inv <- solve(mat)
    x$setInverse(inv)
    
    inv
}
