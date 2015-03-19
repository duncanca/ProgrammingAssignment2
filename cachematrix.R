## The pair of functions below cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
        ## assign matrix 'x' the value passed when calling the function
        ## and assign 'inv' value to NULL, both values cached in the parent environment
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    } 
    get <- function() x                           ## get function returns matrix 'x'
    setinv <- function(inverse) inv <<- inverse   ## cache the value of the inverse matrix 
    getinv <- function() inv                      ## getinv function returns cached inverse of 'x'
    
                                                  ## create a named list of functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix`, if the inverse has already been calculated (and
## the matrix has not changed), then `cacheSolve` should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {                 ## if 'inv' is not null, get cashed value
        message("getting cached data")
        return(inv)
    }
    data <- x$get()                     ## assign matrix value from makeCacheMatrix
    inv <- solve(data)                  ## calculate inverse of matrix
    x$setinv(inv)                       ## assign inverse of matrix to makeCacheMatrix
    inv
}
