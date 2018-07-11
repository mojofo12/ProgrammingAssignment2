## These functions have been written to fulfill the Coursera R Programming
## Week 3 Assignment; due by July 15, 2018; Github user mojofo12

## This function creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by the makeCacheMatrix 
## function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("Getting Cached Data")
          return(inv)
          
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
