## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix takes a square matrix x and returns a special
## matrix that has attached to it the value of it's inverse and two 
## functions to retrieve it and compute it if needed.

## Function cacheSolve computes the inverse of the special matrix created 
## by makeCacheMatrix if it is not already computed, e.g. if the 
## original matrix has changed.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { # Set new matrix x and clear matrix inverse
                x <<- y
                m <<- NULL
        }
        get <- function() x # Get the value of the original matrix x
        setinverse <- function(solve) m <<- solve # Compute the inverse
        getinverse <- function() m # Get the value of the inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() # Get the inverse of x from makeCacheMatrix
        if(!is.null(m)) { # If the inverse is already computed return it
                message("getting cached data")
                return(m)
        }
        # If the inverse is not computed, compute it!
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
