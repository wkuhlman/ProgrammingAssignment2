## This is a solution to Coursera "R Programming" 
## peer-graded programming assignment 2

## Overview:
## These functions create a matrix that has its inverse cached.
## The function makeCacheMatrix generates a cache matrix object.
## The function cacheSolve returns the inverse of a cache matrix object


## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## Usage: <cacheMatrix> <- makeCacheMatrix(<matrix>)

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setSoln <- function(solve) m <<- solve
            getSoln <- function() m
            list(set = set, get = get,
                 setSoln = setSoln,
                 getSoln = getSoln)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
## If the inverse has already been calculated and the matrix has not changed, then
## `cacheSolve` willd retrieve the inverse from the cache.

## Usage: <solvedMatrix> <- cacheSolve(<cacheMatrix>)

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
 
        ## Check to see if the solution is already cached
            m <- x$getSoln()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setSoln(m)
            m
}
