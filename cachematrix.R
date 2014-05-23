    ##  The function makeCacheMatrix creates 
    ##     a matrix object which can cache its inverse.

    makeCacheMatrix <- function(x = matrix()) {
                    m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            ## Built-in function solve calculates the inverse of a matrix
            setinv <- function(solve) m <<- solve
            getinv <- function() m
            ## store the inverse in the object m and save it in cache
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
    }
    
    ## Function cacheSolve computes the inverse of the matrix 
    ##      from the function makeCacheMatrix. 
    ## If the inverse was already calculated before with the same matrix, 
    ##      this function retrieve the calculated value from the cache.
    
    cacheSolve <- function(x, ...) {
                   m <- x$getinv()
            ## Checking if the inverse of the matrix was calculated before
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            ## if it is not in the cache, calculate the inverse
            data <- x$get()
            m <- solve(data, ...)
            x$setinv(m)
            m
    }
