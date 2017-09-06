## cachematrix.R
##
## Caching the Inverse of a Matrix
## 
## VP91

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix 
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function() inv <<- solve(x)
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse()
    inv
}
