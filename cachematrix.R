## Create a function that creates a matrix amd stores its inverse in a cache. 
## Thus if called to invert a matrix, the program will first check if the inverse allready exists in the cache
## - if so it , calls the inverse from the cache rather than recalculating

## This function creates a callable list which stores a set of matrix and its inverse and gets the stored
## matrix as needed

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
            x <<- y
            inv <<- NULL
          }
          get <- function() x
          setinv <- function(solve) inv <<- solve
          getinv <- function() inv
          list( set = set, get = get, setinv = setinv, getinv = getinv)

}


## cachesolve actually computes the inverse of tne matrix x but first
## checks the cache to see if there is a stored value

cacheSolve <- function(x, ...) {
          inv <- x$getinv()
          if(!is.null(inv)) {
                message("getting cache data")
                return(inv)
          }
          mat <- x$get()
          inv <- solve(mat, ...)
          x$setinv(inv)
          inv
}
## Output of inversion
## x <- matrix(1:4, 2, 2)
##  z <- makeCacheMatrix(matrix(x))
## > z$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(makeCacheMatrix(x))
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## z$setinv(solve(x))
## z$getinv()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5