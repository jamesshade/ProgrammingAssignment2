## The functions defined here provide a cache for a matrix and its inverse,
## and a function to calculate the inverse of (solve) a matrix using the cached value
## if it is available.


## Create a matrix cache, initialising the inverse with NULL until setinverse
## is called.  The returned list contains four functions:
##
## get returns the cached matrix.
## set sets the cached matrix (this will also remove any stored inverse).
## setinverse sets the inverse matrix cache
## getinverse gets the cached inverse, returning NULL if it isn't set.

makeCacheMatrix <- function(x = matrix()) {
        cachedinverse <- NULL
        set <- function(newmatrix) {
                x <<- newmatrix
                cachedinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachedinverse <<- inverse
        getinverse <- function() cachedinverse
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Solve (invert) a matrix that has been created using the makeCacheMatrix 
## function.  The inverse of the matrix is provided from the cache if it is 
## available, otherwise the the inverse is calculated (using the solve function)
## and cached for use later.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

