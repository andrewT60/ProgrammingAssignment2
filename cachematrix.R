## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## takes a square invertible matrix as input
## returns a list of 4 functions (f1 f2 f3 f4)
## f1: set the value of the matrix
## f2: get the value of the matrix
## f3: set the value of the inverse of the matrix
## f4: get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL 
        }
        get <- function() x
        setInverse <- function(k) inv <<- k
        getInverse <- function() inv
        list(set=set, get=get,
                setInverse = setInverse,
                getInverse = getInverse)
}

## Write a short comment describing this function
# This function returns the inverse of the matrix. 
# First, if the inverse has already been computed, it gets the result 
# and skips the computation.
# Otherwise, it computes the inverse, sets the value in the cache using
# the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inv <- x$getInverse()
                if (!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data)
                x$setInverse(inv)
                inv
}
