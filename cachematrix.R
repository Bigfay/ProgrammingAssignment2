## Caching the inverse of a matrix
## When computing repeatdely inverse of a matrix it can be time consumming.In order to save 
## computation time when possible, those 2 functions allow storing matrix and inverse matrix
##  result in cache. Also, they allow to skip retrieve result stored in cache if already existing.
## Otherwise, computation is done.

## makeCacheMatrix function creates a special vector to cache the matrix,its inverse value 
## and retrieve them on request

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        setmat <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmat <- function() x
        
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(setmat = setmat, getmat = getmat,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function computes the inverse of a matrix but prior to that checks the cache
## to make sure not already computed. If so it returns the value residing in cache.
## As requested, matrixes submitted are assumed to be inversible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmat()
        m<- solve(data, ...)
        x$setinv(m)
        m
}
