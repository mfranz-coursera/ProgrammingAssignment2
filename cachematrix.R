## Put comments here that give an overall description of what your
## functions do

## Creates a matrix wrapper that will cache the inverse matrix once the inverse matrix is requested.
## There is no error checking and the matrix must be valid to determine the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        cachedinverse <- NULL
        set <- function(y) {
                x <<- y
                cachedinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachedinverse <<- inverse
        getinverse <- function() cachedinverse
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return the inverse matrix.  A cached version of the matix is return if exists, otherwise calculate, store
## and return the inverse matrix.

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse        
}
