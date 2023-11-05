

## Create the "makeCacheMatrix" function that can create a matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(matrix) {
                m <<- matrix
                s <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) s <- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## "cacheSolve computes the inverse of the object returned by the
## "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
        