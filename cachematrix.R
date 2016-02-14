## This function will create a object that can cache a matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function will compute the inverse of the matrix created on makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
    if (!is.null(inv)) {
        return(inv)
    }
    matriz <- x$get()
    inv <- solve(matriz, ...)
    x$setInverse(inv)
    inv
}
