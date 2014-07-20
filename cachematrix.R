## The script is intended to provide a matrx-like object
## whose inverse can be cached. This allows us to bypass memory intensive
## computation if we need to repeatedly calculate a matric'es inverse
## without changing it.
## There are two functions, makeCacheMatrix() and cacheSolve()
## The aim of these two functions are to provide a wrapper
## around a matrix object, which allows us to have a custom interface

## This function takes a matrix as an argument and returns a list
## The returned list contains functions that allow the caller to
## cache an inverse value and to retrieve it

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Cached inverse value, initialized
        set <- function(y) { # Setter
                x <<- y
                ## When we set/change the value, cache is reset
                inv <<- NULL
        }
        get <- function() x # Getter
        setinverse <- function(i) inv <<- i # Set cache
        getinverse <- function() inv # Get cache
        ## Return a list with only the named functions
        ## Access to the internal objects x & inv not allowed directly
        ## Thus we completely control the interface
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function handles the matrix inverse calculation
## If the cache is already set, it will retrieve the inverse and return it
## If not, it will calculate the inverse, set the cache and then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() # Retrieve cached inverse
        if (!is.null(inv)) {
                ## message("Getting cached data...")
                return(inv)
        }
        ## Not cached yet
        mat <- x$get() # Get the internal matrix object
        inv <- solve(mat, ...)
        x$setinverse(inv) # Set cache
        inv
}
