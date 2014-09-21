## This pair of functions cache the inverse of a matrix. If the inverse has not yet been calculated, they will calculate and store it
## If the inverse has previously been calculated, it will simply be looked up and returned
## changing the matrix resets a marker so the program will know to recalculate it on next need.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## when created or changed, set the marker m so we know to recalculate inverse
    m <- NULL
    set <- function(Y) {
        X <<- Y
        m <<- NULL
    }
    
    ## these functions allow pulling and pushing pre-solved inverses
    get <- function() X
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    
    ## set the function definitions for the object
    list(set = set, get = get, setInv = setInv, getInv = get Inv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- X$setInv()
    
    ## check if the inverse has already been calculated
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if it has not, calculate it and set the marker so we knwo to skip next time
    data <- X$get()
    m <- solve(data, ...)
    X$setInv(m)
    m
}
