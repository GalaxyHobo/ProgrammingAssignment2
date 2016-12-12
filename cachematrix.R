## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
##
## By LB, 12 Dec 2016
##
## Instructions: Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The assignment is to write a pair of functions that cache the  
## inverse of a matrix.

## This function creates a special "matrix" object, which is really a list of 
## functions for handling the matrix, including making a cache of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    } 
    setInv <- function(invMat) {
        m <<- invMat
    }
    getInv <- function() {
        m
    }
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
cacheSolve <- function(x) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached matrix")
    }
    else
    {
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
    }
    m 
}

