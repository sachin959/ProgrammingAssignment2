## this functions here caches the Inverse of a matrix which can be used later without recomputing the 
## the inverse again
## makeCacheMatrix contains is the core function and contain four subfunctions
## set : to set the data for calculating the inverse
## get : retrieve the matrix whose inverse is calculated i.e. original matrix
## getInv : getthe cached Inverse Matrix
## setInv : cache the Calculated Inv for later use

## At first Inverse Matrix is null
## later on first retrieval value is set.

makeCacheMatrix <- function(x = matrix()) {
    res <- NULL
    set <- function(y){
        x <<- y
        res <<- NULL
    }
    get <- function() x
    getInv <- function() res
    setInv <- function(z){
        res <<- z
    }
    list(set = set,
         get = get,
         getInv = getInv,
         setInv = setInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getInv()
    ## if mInv is not null than the cached matrix is returned
    if(!is.null(mInv)){ 
        print("getting Cached Matrix");
        mInv
    }
    ## if mInv is null Inverse has not yet been calculated so first
    ## its calculated and then this matrix is returned
    else{
        z <- x$get();
        z <- solve(z);
        x$setInv(z);
        z
    } 
}
