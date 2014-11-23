## The following piece of code defines two functions.
## The function makeCacheMatrix creates a special matrix
## that can cache its inverse.
## The function cacheSolve returns the inverse of a matrix
## if it is already cached.

## The function makeCacheMatrix creates a special "matrix",
## which is essentially a list of the following four functions:
## (1) 'set' caches the matrix y
## (2) 'get' is a function that simply returns x
## (3) 'setinverse' is a function that takes a matrix
##      inverse, and caches it.
## (4) 'getinverse' returns the inverse, inv
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }        
        get <- function() x        
        setinverse <- function(inverse) inv <<- inverse        
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve returns the inverse of the
## special "matrix" created by makeCacheMatrix. The
## function first checks if the inverse has been cached,
## and returns it if that is the case. Otherwise, it
## computes the inverse and caches it, using setinverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
