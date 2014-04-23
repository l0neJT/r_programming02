## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invertX <- NULL
    
    ##
    set <- function(y) {
        x <<- y
        invertX <<- NULL
    }
    
    ##
    get <- function() x
    
    ##
    clear <- function() set(matrix())
    
    ##
    setInvert <- function() invertX <<- solve(x)
    
    ##
    getInvert <- function() invertX
    
    ##
    clearInvert <- function() invertX <<- NULL
    
    ##
    list(set = set, get = get, clear = clear, setInvert = setInvert,
         getInvert = getInvert, clearInvert = clearInvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    if(is.null(x$getInvert())){
        message("creating inverse matrix")
        x$setInvert()
    } else {
        message("retrieving from cache")
    }
    x$getInvert()
}