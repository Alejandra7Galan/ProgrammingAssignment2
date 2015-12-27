## This function help to cache  potentially time-consuming computations.
## Matrix inversion is usually a costly computation  
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it. 

## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse. This function include the next functions:
## setMatrix is a function to set the values of a matrix.
## getMatrix is a function to get the values of a matrix.
## setInverse is a function to value of the inverse of the matrix.
## getInverse is a function to get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

## initially nothing is cached so set it to NULL    
    invers <- NULL

## Set a new matrix    
    setMatrix <<- function(y) {
        x <<- y
        ## since the matrix is assigned a new value, flush the cache
        invers <<- NULL 
    }
    getMatrix <- function() x ##Get nthe new matrix
    setInverse <- function(solve) invers <<- solve ## set the inverse of the matrix
    getInverse <- function() invers ##Get the inverse of the matrix
    
    # return a list where each named element of the list is a function
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse
         getInverse = getInverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        invers <- x$getInverse  ##get the cached value
        if(!is.null(invers)){
            message("getting cache data") ## if a cached value exists return the message
            return(invers) # and the cached value
        }
        data <- x$get() ## otherwise get the matrix, caclulate the inverse 
        invers <- solve(data, ...) ## and store it in the cache
        x$setInverse(invers)
        invers ##return the inverse of the matrix
}
