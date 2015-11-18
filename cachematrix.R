## Functions to create a special structure to matrix and calculate it's Inverse

## Function that creates a special structure to cache a matrix and it's Inverse

makeCacheMatrix <- function(matrix = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        matrix <<- y
        matrixInverse <<- NULL
    }
    get <- function() matrix
    setInverse <- function(inverse) matrixInverse <<- inverse
    getInverse <- function() matrixInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function that calculates the Inverse of the matrix and cache it 
## in the special structure

cacheSolve <- function(matrix, ...) {
    matrixInverse <- matrix$getInverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    data <- matrix$get()
    matrixInverse <- solve(data)
    matrix$setInverse(matrixInverse)
    matrixInverse
}
