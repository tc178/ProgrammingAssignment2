## The purpose of this function is to create a special matrix that can 
## cache its inverse

## Write a short comment describing this function

 setwd('C:/Users/tomcr/Documents/Coursera-R')

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Now this function computes the inverse of the above function
## The purpose of cacheSolve is to retrieve the inverse of the Cache 

cacheSolve <- function(x, ...) {
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
