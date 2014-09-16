## cachematrix.R

## Cache the inverse of a square matrix for improved performance.
## 
## Usage Example:
##
##  > inputMatrix <- matrix(somedata, nrow = 100, ncol = 100)
##
##  > cm <- makeCacheMatrix(inputMatrix)
##  > cacheSolve(cm)     ## calcs & outputs inverse matrix
##  > cacheSolve(cm)     ## quickly outputs previous result from cache


## Set up a cacheMatrix object to contain cached inverse result
## Object contains appropriate access/assign methods

makeCacheMatrix <- function(x = matrix()) {

     inv <- NULL  ## no inverse result yet
     
     ## set functions require <<- operator to "scope up" to main object
     set <- function(y) { 
          x <<- y
          inv <<- NULL   ## changing the original, so clear any prior result
     }
     get <- function() { x }
     
     setinverse <- function(inverse) {
          inv <<- inverse
     }
     getinverse <- function() { inv }
     
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'
## Check the cacheMatrix for previous result
## Perform calculation if previous result not available.

cacheSolve <- function(x, ...) {
     
     ## x assumed to be a cacheMatrix containing an invertible matrix
     ## otherwise, this will fail
     
     m <- x$getinverse()
     
     if(is.null(m)) {    ## not cached, so generate here
          orig <- x$get()
          m <- solve(orig, ...)  
          x$setinverse(m)     ## cache for next time
     }
     else {
          message("Using cached data...")
     }

     return (m)
     
}
