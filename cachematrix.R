## Matrix inversion is usually a costly computation and there may be a benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## This file contains a pair of functions that cache the inverse of a matrix.


## Create a special "matrix" object that encapsulates an actual matrix and
## can cache the inverse of that matrix. Example use:
##      amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##      amatrix$get()         # Returns original matrix
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##      amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
##  After calling cacheSolve(amatrix), one may then call
##      amatrix$getinverse()  # Returns matrix inverse
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedInverse <<- inverse
    getinverse <- function() cachedInverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve will retrieve the inverse from the cache.
## Example use:
##      cacheSolve(amatrix)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inputMatrix <- x$get()
    inverse <- solve(inputMatrix, ...)
    x$setinverse(inverse)
    inverse
}
