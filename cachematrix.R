## Calculate the inverse of a matrix, and recall it if it's already been calculated.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) m <<- matrix
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix
             )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        m <- x$getMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get
        m <- solve(matrix, ...)
        x$setMatrix(m)
        m
}