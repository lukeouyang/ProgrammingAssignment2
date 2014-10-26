## This set of two functions is aiming to reducing the high cost of computing the inverse of any numeric matrix,
## by caching the inverse of a matrix rather than compute it repeatedly.

## This function, "makeCacheMatrix()", creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inver <- NULL  ## Declare and initialize the variable 'inver' to store the inverse of matrix argument 'x'

## Function set() will set the matrix 'x' to be identical to the input argument 'y'
set <- function(y = matrix()){
        x <<- y
        inver<<- NULL
}

## Function get() will return the current matrix 'x'
get <- function() x

## Function setinverse() will set the inverse "inver" to be identical to input argument "inverse"
setinverse <- function(inverse) inver <<- inverse

## Function getinverse will return the current inverse
getinverse <- function() inver

## Return the list of available functions
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function, "cacheSolve()", computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## update the local variable "inver" to be identical to the inverse of input argument 'x'
        inver <- x$getinverse()
        
        ## If the inverse had already been cached, return it
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        
        ## If the inverse had not already been cached, compute and cache it
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
}
