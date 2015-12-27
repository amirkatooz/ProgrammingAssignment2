## Functions below are used to create a special object that stores a matrix
## and cache's its inverse.

## makeCacheMatrix is a function which creates a special matrix object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## casheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated, the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached adta")
        return(i)
    }
    data <-x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
