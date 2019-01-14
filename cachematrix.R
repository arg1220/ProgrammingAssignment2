## These functions are made to create an object that stores a matrix
## and to then cache the inverse of said matrix.

## This function creates a matrix object containing information to cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y){
                x <<- y
                invrs <<- NULL
}
get <- function() x
setinverse <- function(solveMatrix) invrs <<- solveMatrix
getinverse <- function() invrs
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the above matrix after checking to see if the inverse
## has been calculated already. If it's been calculated before, the inverse is taken from the cache
## and no further computation is done.

cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        if(!is.null(invrs)){
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data)
        x$setinverse(invrs)
        invrs
}
