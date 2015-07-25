## Write R functions that are able to cache potentially 
## time-consuming computations.

## Function that creates a special matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
          x <<-y
          inverse <<- NULL
    }
    get <- function() x
    setinverse <-function(inversebis) inverse <<- inversebis
    getinverse <- function() inverse
    list( set = set, get=get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## Function that computes the inverse of the special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)){
        message("getting cached data")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
}
