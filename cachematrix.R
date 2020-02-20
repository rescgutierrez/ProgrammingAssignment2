## makeCacheMatrix can store a matrix and its inverse, and has functions for 
## accessing and changing them; cacheSolve returns the inverse of the matrix stored in a makeCacheMatrix 
## instance, but first checks if the inverse isn't already stored to avoid recalculating.
## If the data of the 'matrix object' created by makeCacheMatrix is changed, makeCacheMatrix.set() erases the inverse to avoid incongruence.

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
                    x <<- y
                    inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv   
    
}
