## The makeCacheMatrix and cacheSolve functions are used to create "special" matrices that have
## the ability to cache their inverses once calculated to avoid costly repetitive calculations

## The makeCacheMarix creates a list of four functions to set the value of the matrix, get the 
## value of the matrix, set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <<- NULL
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


## The cacheSolve function calculates the inverse of the special matrix created with the above
## function. However, it first checks the cache to see if the inverse has already been calculated.
## If not, it calculates the inverse and sets its value in the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
