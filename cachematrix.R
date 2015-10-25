## The two functions are used to create a special list 
## that store a matrix and cache its inverse.

## makeCacheMatrix creates a 'matrix' that can cache its inverse, 
## which is actually a list
## containing set/get the value of the matrix/inverse.
makeCacheMatrix <- function(x = matrix()) {
        #initiate an empty inv
        inv <- NULL
        #store a matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #return the stored matrix
        get <- function() x
        #cache the matrix and get the inverse
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        #return a list containing set/get the value of the matrix/inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Based on makeCacheMatrix, cacheSolve computes theinvere of
## the 'matrix' list. If the inverse has been calculated, then
## the function will get it from cache; otherwise, it will use solve()
## to do it again.

cacheSolve <- function(x, ...) {
        #Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        #if the inverse exists, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #if not, get the matrix
        mat <- x$get()
        # calculate the inverse
        inv <- solve(mat, ...)
        x$setInverse(inv)
        #return the inverse
        inv
}
