## This is a pair of functions that cache the inverse of a matrix.
## 

## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
        set1 <- function(y) {
                x <<- y
                i <<- NULL
        }
        get1 <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set1 = set1,
             get1 = get1,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get1()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
