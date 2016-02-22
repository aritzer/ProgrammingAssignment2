## The following function create a matrix object and will cache the inverse 
## of that matrix. When calculating the inverse of the matrix, the function
## check for a cached solution before calculating the inverse. Using caching
## greatly decreases the computation time for repeated calls for the matrix
## inverse.

## This function creates a matrix object that can cache its inverse.,
## The function is able to: set the value of the matrix, get the value of the
## matrix, set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of a matrix created in the 
## makeCacheMatrix function. It checks for a cached result, before calculating
## the inverse. If the cache exists, it is retreived.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
