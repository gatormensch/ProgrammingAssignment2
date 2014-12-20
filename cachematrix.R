## Dan Feldman
## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## Here follows a pair of functions that cache the inverse of a matrix.
## Computing the inverse of a square matrix can be done with the solve function 
## in R. For example, if X is a square invertible matrix, then solve(X) 
## returns its inverse.

## Write a short comment describing this function
## This function creates a special object, that stores a matrix and
## caches its inverse.
## The first function, makeCacheMatrix creates a special "vector", which is a 
## list containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize an empty inverse matrix
        i <- NULL
        ## 1) set the value of the matrix
        set <- function(y) {
                ## Specifically set the value of the function to an object in an
                ## environment that is different from the current environment.
                x <<- y
                i <<- NULL
        }
        
        ## 2) get the value of the matrix
        get <- function() x
        
        ## 3) set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
        
        ## 4) get the value of the inverse
        getinverse <- function() i
        
        ## Returns a "vector", which is list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the 
## matrix has not changed), then the `cachesolve` should retrieve the inverse 
## from the cache. If the already been calculated (and the matrix has not 
## changed), then `cacheSolve` should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the `solve`
## function in R. For example, if `X` is a square invertible matrix, then
## `solve(X)` returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## return the cached inverse, if the cache is not empty
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Otherwise, calculate the inverse of the data and 
        ## set the value of the inverse in cache via the setinverse function.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
