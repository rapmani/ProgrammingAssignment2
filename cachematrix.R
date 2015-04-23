## In this file you have too functions. The first makes a special
## 'matrix' object that can cache the inverse of a matrix. With
## the second function, you can calculate the inverse (using the
## R-built solve() function.)

## makeCacheMatrix

# First function 'makeCacheMatrix' creates a list with four elements.
# This function creates a special "matrix" object that can cache its inverse:
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of a matrix
# 4.  get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## 'cachesolve'

# The second function 'cacheSolve' computes the inverse of the special
# "matrix" returned by 'makeCachematrix' above. The setinverse function
# from above is called as you can see in the last lines.
# If the inverse has already been calculated (and the matrix has not
# changed), then 'cacheSolve' retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
