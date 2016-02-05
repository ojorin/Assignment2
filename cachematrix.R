## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a list containing a
## function to 
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() x
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## The function first checks to see if the inverse
## has already been calculated (and the matrix has not changed). 
## Then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
