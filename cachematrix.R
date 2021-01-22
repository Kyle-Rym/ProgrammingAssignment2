## These are functions that enables the caching of an inverse matrix.

## This function creates a special matrix that will cache any inverse

makeCacheMatrix <- function(x = matrix()) {
## Start to convert to inverse matrix
    i <- NULL

    ## Function to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
}
## Function to get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Function to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Function to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## This allows to return the functions used
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Afterwards, the inverse of the matrix is shown by "makeCacheMatrix"
## above. If the inverse is already calculated , 
## then the "cachesolve" will retrieve it instead.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from the object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Finally, return the matrix
    m
}

