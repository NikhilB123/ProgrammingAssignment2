## Caching the Inverse of a Matrix:
## Matrix inversion is typically a time-consuming computation so the following
## pair of two functions create a matrix object that stores the matrix itself and
## caches its inverse to calculate it more effeciently

## This function creates a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseVar <- NULL
    set <- function(y) {
        x <<- y
        inverseVar <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseVar <<- inverse
    getInverse <- function() inverseVar
    list(set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## This function, depening on if the inverse has already been calculated,
## calculates the inverse of the matrix object and retrieves the inverse from
## the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseVar <- x$getInverse()
    if (!is.null(inverseVar)) {
        message("getting cached data")
        return(inverseVar)
    }
    matrixVar <- x$get()
    inverseVar <- solve(matrixVar, ...)
    x$setInverse(inverseVar)
    inverseVar
}
