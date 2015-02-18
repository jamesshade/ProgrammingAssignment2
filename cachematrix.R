## The functions defined here provide a cache for a matrix and its inverse,
## and a function to calculate the inverse of (solve) a matrix using the cached value
## if it is available.


## Create a cached matrix, initialising the inverse with NULL until setinverse
## is called.

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(newmatrix) {
                m <<- newmatrix
                i <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solve a matrix created using the makeCacheMatrix function.  The inverse of the 
## matrix is provided from the cache if it is available, and if it is not,
## the inverse is calculated (using the solve function) and cached with the matrix
## for use later.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

