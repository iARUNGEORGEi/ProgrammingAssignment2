## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         matrix_inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matrix_inverse <<- inverse
        getInverse <- function() matrix_inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        mat <- x$get()
        matrix_inverse <- solve(mat, ...)
        x$setInverse(matrix_inverse)
        matrix_inverse
}
