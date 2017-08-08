## Objective: To create a pair of functions that cache the inverse of a matrix

# When running time consuming computations, it's good to cache the results so that you can look
# them up later instead of computing them again. For example, maxtrix inversion is usually costly, 
# especially when running inside of a loop. The following functions can compute and cache the 
# inverse of a matrix.

## The first function: makeCacheMatrix
### Input: a square invertible matrix, x
### Return: a list containing functions to -
####       1. set the matrix, setMatrix
####       2. get the matrix, getMatrix
####       3. set the inverse, setInverse
####       4. get the inverse, getInverse

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        setMatrix <- function(y){
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) inv <<- inverse 
        getInverse <- function() inv
        list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## The second function: cacheSolve
### Input: the output from makeCacheMatrix(), taken as x
### Return: inverse of original matrix input to makeCacheMatrix()
### If the inverse is already calculated, retrieve from cache and skip computation
### otherwise, compute the inverse of matrix

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$getMatrix()
        inv <- solve(data, ...)
        x$setInverse(inv)
        
        return(inv)
}
