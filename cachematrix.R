makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is a square invertible matrix
        ## the function lays the groundwork to set the matrix, get the matrix, set the inverse, and get the inverse
        inverse <- NULL
        set <- function(y) {
                ## set the matrix
                x <<- y
                m <<- NULL
                ## use the operator '<<-' to assign a value to an object in an environment that is different from the current environment
        }
        get <- function() x  ## get the matrix
        setinverse <- function(inverse) inverse <<- inverse ## set the inverse
        getinverse <- function() inverse ## get the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## 'x' is the output of makeCacheMatrix()
        inverse <- x$getinverse()
        ## inverse of the original matrix input to makeCacheMatrix()
        if(!is.null(inverse)) {
                ## 'if' refers to if the inverse has already been calculated
                message("getting cached data")
                ## obtain data from the cache
                return(inverse)
        }
        matrixdata <- x$get()
        inverse <- solve(matrixdata, ...)
        ## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        ## if the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
        
        x$setinverse(inverse) ## sets the value of the inverse in the cache
        inverse ## Return a matrix that is the inverse of 'x'
}