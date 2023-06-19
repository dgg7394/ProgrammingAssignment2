## Two functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
        
    ## Initialize inverse property
    i <- NULL

    ## Set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Returns the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get matrix from our object
    data <- x$get()

    ## Calculate inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set inverse to the object
    x$setInverse(m)

    ## Return matrix
    m
}
