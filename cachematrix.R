## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function below calculates the inverse of a matrix and saves it to the cache

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
        
## Write a short comment describing this function
## The function below calculates the inverse of the matrix created by the makeCacheMatrix function
## cacheSolve looks to see if the inverse was previously calculated
## If so, it skips the calculation
## If not, it calculates the inverse of the matrix and sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
}

