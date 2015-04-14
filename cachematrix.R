## The functions makeCacheMatrix and cachSolve are used to create a matrix object
## that can cache its inverse.  This is done to save computation time by caching the inverse.

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to 
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse
## 4. gets the values of the inverse (assumes it is invertible)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created
## with makeCacheMatrix.  It will first check if the inverse has been previously
## calculated (cached). If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the values of the inverse
## in the cache via the setinv function.   Assumes the matrix is invertible.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
