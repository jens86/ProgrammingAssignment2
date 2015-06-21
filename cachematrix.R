## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix contains a function to do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invMatrix <<- inverse
    getinverse <- function() invMatrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

##For this assignment, assume that the matrix supplied is always invertible.

## The following function calculates the inverse of a matrix.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of 
## the matrix in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getinverse()
    if(!is.null(invMatrix)) {
        message("getting cached data.")
        return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data)
    x$setinverse(invMatrix)
    invMatrix

}
