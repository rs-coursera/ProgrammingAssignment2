## The code below implements Cached approach 
## of inverse ## matrix calculation.
## Sample test code:
## source("cachematrix.R")
## t<-matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
## t1<-makeCacheMatrix(t)
## cacheSolve(t1)
## cacheSolve(t1)
## Second call returns the same inverse matrix
## from cache. Additional message is printed.

## Function creates a 'special matrix', 
## which is list of functions related to the provided matrix.
## Theses functions allow to store and retrieve
## the matrix and corresponding inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invMatrix <<- inverse
    getinverse <- function() invMatrix
    
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Function returns inverse matrix for
## the provided 'special matrix'. 
## If the inverse matrix was calculated before, 
## cached value is returned and message is printed.
## Otherwise, inverse is calculated and added to cache.
## Internally it uses functions from 'special matrix' list.

cacheSolve <- function(x, ...) {
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
