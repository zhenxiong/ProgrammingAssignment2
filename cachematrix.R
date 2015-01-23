## Overall Function: There are two functions within this script. 
## makeCacheMatrix() turns a typical matrix into a special matrix that allows caching.
## cacheSolve() returns the inverse of the matrix input.

## makeCacheMatrix() accepts a non singular matrix as input and 
## turns the input into a "special matrix" that allows caching, 
## and retriving of the cached info i.e. inverse of input.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve() accepts a "special matrix" as input and
## retrieves and returns the inverse of the special matrix from the cache if it exists.
## Otherwise, the inverse of the "special matrix" will be calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
