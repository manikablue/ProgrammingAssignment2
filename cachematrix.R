## Put comments here that give an overall description of what your
## functions do

## This function returns a list to of functions; These functions are setting a matrix,getting a matrix,setting the inverse of the matrix
## and getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function retrieves the cached value of the inverse of a matrix, else calculates it and caches it for later use

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
