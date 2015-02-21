## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly

makeCacheMatrix <- function(x = matrix()) {
        
        ## Creates a list of functions that can cache the inverse of a matrix
        
        inv <- NULL
        set <- function(y) {
                ## Sets the argument to X and makes available globally
                x <<- y
                ## Sets inv to NULL globally
                inv <<- NULL
        }
        ## creates a function to return x
        get <- function() x
        ## creates a function to calulate inverse and makes this globally available in "inv"
        setinverse <- function(solve) inv <<- solve 
        ## creates a function to retrieve "inv"
        getinverse <- function() inv
        ## creates and returns a vector
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x=matrix(), ...) {
        
        ## Computes the inverse of the matrix returned
        ## by makeCacheMatrix(), unless the inverse has
        ## already been calculated, in which case
        ## it retrieves it from the cache
        
        inv <- x$getinverse()
        ## Need to check if x$get() is the same as the argument? (ie has matrix changed?)
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinverse(inv)
        inv
}

## Test script

x <- matrix(c(45, 34, 3, 41, 5, 6, 77, 81, 9), nrow=3, ncol=3)
x <- makeCacheMatrix(x) ## change back to z <- ...

answer <- cacheSolve(z)
answer

## Repeat the call
answer <- cacheSolve(z)
answer

## Now change x
x <- matrix(c(45, 34, 3, 41, 5, 6, 77, 81, 99), nrow=3, ncol=3)
## answer shouldnt be the cached one
answer <- cacheSolve(z)
answer
