##  cache the inverse of a matrix

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setCacheMatrix <- function(cacheMatrix) c <<- cacheMatrix
        getCacheMatrix <- function() c
        list(set = set, get = get,
             setCacheMatrix = setCacheMatrix,
             getCacheMatrix = getCacheMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        c <- x$getCacheMatrix()
        if(!is.null(c)) {
                message("getting cached matrix")
                return(c)
        }
        data <- x$get()
        c <- solve(data, ...)
        x$setCacheMatrix(c)
        c
}
