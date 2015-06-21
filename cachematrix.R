## According to the given example for Caching the Mean of a Vector
## given in the R programming module of the data science specilization
## one can modify accordingly for the case of the chaching of a matrix
## and the calculation of its inverse. All comments below are also
## modified accordingly to reflect the case examined here
## (e.g. matrix inversion instead of vector mean calculation).

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix's inverse
## 4. get the value of the matrix's inverse

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
                }
        get <- function() mat
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
        }

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. f so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via
## the setinverse function. 

cacheSolve <- function(mat, ...) {
        inv <- mat$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }
        data <- mat$get()
        inv <- solve(data, ...)
        mat$setinverse(inv)
        inv
}
