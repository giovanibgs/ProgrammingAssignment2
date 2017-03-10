## This function creates a matrix and cache it inverse
## It will set the matrix and get the matrix
## set the inverse and get the inverse
## the list is a inout ti cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function
## return: inverse of the original matrix input to makeCacheMatrix()
# if the inverse has already been calculated
# otherwise, mat calculates the inverse 

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
