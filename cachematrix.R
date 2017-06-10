# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # initial variable m for storing inversed matrix
    m <- NULL

    # set original matrix
    set <- function(y) {
        # update matrix
        x <<- y
        m <<- NULL
    }
    # get original matrix
    get <- function() x
    # set inversed matrix
    setinversed <- function(inversed) m <<- inversed
    # get inversed matrix
    getinversed <- function() m
    # binding functions to list
    list(set = set, get = get,
         setinversed = setinversed,
         getinversed = getinversed)
}


# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    # read inversed matrix
    inversed <- x$getinversed()
    # if there is a cached inversed matrix
    if(!is.null(inversed)) {
        message("getting cached data")
        # return cached inversed matrix
        return(inversed)
    }
    # get original matrix
    matrix <- x$get()
    # inverse matrix
    inversed <- solve(matrix, ...)
    # cache inversed matrix
    x$setinversed(inversed)
    # return inversed matrix
    inversed
}
