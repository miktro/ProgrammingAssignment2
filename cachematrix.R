## Put comments here that give an overall description of what your
## functions do

## This creates a new object that stores a matrix 
## together with it's inverse - initially set to NULL

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) mi <<- inv
    getinverse <- function() mi
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks supplied object to see if it has an inverse already.
## If there is an inverse then it is returned.
## Otherwise the inverse is 'solve'd, stored for furture use and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mi <- x$getinverse()
    if(!is.null(mi)) {
        #message("getting cached matrix inverse")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setinverse(mi)
    mi
}
