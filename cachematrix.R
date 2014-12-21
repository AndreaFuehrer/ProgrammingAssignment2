## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It's really a list containing a function to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse of the matrix
## (4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) z <<- solve
    getSolve <- function() z
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    z <- x$getSolve()   ## z is set to the inverse matrix
    ## if the inverse matrix exists the message "Getting cached data" is 
    ## displayed and the inverse matrix is returned
    if(!is.null(z)) {   
        message("Getting cached data")
        return(z)
    }
    ## if the inverse matrix does not exist it is calculated, stored in the special
    ## matrix created by makeCacheMatrix and is returned
    data <- x$get()
    z <- solve(data, ...)
    x$setSolve(z)
    z
}

