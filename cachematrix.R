## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCackeMatrix function  creates a special "matrix "object that can 
## store the matrix and its inverse in function's environment 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

## The cacheSolve function first checks either inverse of the provided 
## "matrix" object has already been calculated. If yes, than inversed matrix is retrieved from cahce and returned,
## otherwise function computes the inverse and "caches" it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
