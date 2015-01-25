## The functions makes a matrix that can cache its inverse,
## then, if the inverse of the same matrix has already been calculated,
## it is retrieved from the cache.
## Otherwise calculate it de novo.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv

        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse of the same matrix has already been calculated,
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
