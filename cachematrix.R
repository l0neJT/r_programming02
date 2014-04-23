## Logan J Travis
## github.com/l0neJT
## 2014-04-23

## 'makeCacheMatrix' and 'cacheSolve' simplify data management for
## a primary matrix and its inversion by keeping both in memory,
## limiting access through a list of control functions, and by checking
## the cached matrices prior to solving for the inversion.


## store and manage a matrix along with its inversion
## keeps both in memory for retrieval without reprocessing
## returns a list of control functions (e.g. get, set, clear, etc.)
## NO DIRECT ACCESS to cached matrices
makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    
    ## store a new primary matrix and reset the inversion matrix
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    
    ## return the primary matrix
    get <- function() x
    
    ## reset both the primary and inversion matrices; for testing
    clear <- function() set(matrix())
    
    ## store the inverstion of matrix 'x' to 'invX'
    ## calls 'solve' from the R base package
    ## returns an error if no solution possible
    solveInv <- function() invX <<- solve(x)
    
    ## return inversion matrix
    getInv <- function() invX
    
    ## reset inversion matrix to NULL; for testing
    clearInv <- function() invX <<- NULL
    
    ## return the list of avaialble function calls
    list(set = set, get = get, clear = clear, solveInv = solveInv,
         getInv = getInv, clearInv = clearInv)
}


## take a 'makeCacheMatrix' object and return the inversion matrix
## checks memory for a non-NULL inversion matrix either returning the
## stored matrix or - if NULL - solving first then returning the matrix
cacheSolve <- function(x, ...) {
    if(is.null(x$getInv())){
        message("solving inverse matrix")
        x$solveInv()
    } else {
        message("retrieving from cache")
    }
    x$getInv()
}