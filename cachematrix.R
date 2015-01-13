## Code written by Laura Lazzati

## makeCacheMatrix creates a list that cointains the original matrix that is passed as the x argument and 
## the solve function that will be used to calculate its inverse. Also, both the matrix and the solve 
#function can be obtained if wanted.
#Note: the matrix should be squared and either numeric or complex.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve gets the matrix created in the previous function (which result is actually a list that 
#contains both the matrix and the solve function to apply to it) and calculates its inverse. If the
# inverse matrix is already calculated, cacheSolve obtains it from the object already mentioned. 
#Otherwise, it calculates the inverse and also stores it in the matrix cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cached inverse!")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}
