## This fuction does perform matrix inverse operation.

makeCacheMatrix <- function(x = matrix()) {                          ##Function Call.
         matrix_inverse <- NULL                                      ##Null being assigned to a variable matrix_inverse
        set <- function(y) {                                         ##A nested function is created.
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x                                          
        setInverse <- function(inverse) matrix_inverse <<- inverse   ## Matrix inverse operation.
        getInverse <- function() matrix_inverse                      
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

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
