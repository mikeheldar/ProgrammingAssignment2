## Methods: makeCacheMatrix, cacheSolve
## makeCacheMatrix creates a special cacheable "matrix"
##     This is done by defining accessor methods to get and set matrix data
##     and setting methods to get and set the inverse matrix
##
## cacheSolve calculates the inverse of a matrix x or gets cached inverse
##     This is done by first checking if matrix x already has a cached inverse
##     If it does the cached inverse is returned, if not solve() creates one

## makeCacheMatrix creates a special cacheable "matrix"

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse i for matrix x to NULL
    i <- NULL
    
    ## define set function on matrix x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## define get function on matrix x
    get <- function() x
    
    ## define setinverse function on matrix x
    setinverse <- function(inverse) i <<- inverse
    
    ## define getinverse function on matrix x
    getinverse <- function() i
    
    ## return list of methods for matrix x to serve as cached matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix x or gets cached inverse

cacheSolve <- function(x, ...) {
    ## First get cached inverse if it was already cached
    i <- x$getinverse()
    if(!is.null(i)) {
        ## print message to user that inverse data was cached
        message("getting cached inverse data")
        ## return cached inverse
        return(i)
    }
    data <- x$get()
    
    ## print message to user that inverse is being created
    message("creating inverse data")
    ## solve function used to create inverse
    i <- solve(data)
    
    ## set the inverse on the matrix
    x$setinverse(i)
    
    ## return the inverse
    i
}

