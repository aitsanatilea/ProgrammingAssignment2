## Pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinverted <- function(invertedx) invx <<- invertedx
    getinverted <- function() invx
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getinverted()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setinverted(invx)
    invx
}
