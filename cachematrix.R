## These functions create a matrix and store the inverse in the cache. If the inverse has not been stored by the makeCacheMatrix function, cacheSolve will calculate the inverse.
## 

## This function creates a matrix and stores its inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse=setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix or takes the inverse from the cache if already calculated.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
          if(!is.null(inv)){
            message("getting cached data")
            return(inv)
          }
          mat <-  x$get()
          inv <- solve(mat, ...)
          x$setInverse(inv)
          inv
       
}
