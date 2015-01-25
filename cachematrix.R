## makeCacheMatrix creates a special type of matrix that caches its own inverse.
## cacheSolve returns the inverse of X.


## This function constructs a special type of matrix that has ability to cache its own inverse.
## It also provides accessors and  mutators to both the matrix and its inverse.
makeCacheMatrix <- function(X = matrix()) {

        iX <- NULL
        set <- function(Y) {
                X <<- Y
                iX <<- NULL
        }
        get <- function() X
        setInverse <- function(inverseX) iX <<- inverseX
        getInverse <- function() iX
}


## This function checks if the X has inverse of it already cached. If yes, it returns the same.
## Otherwise, it calculates the inverse of X and caches it in X. 
cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
        iX <- X$getInverse()
        if(!is.null(iX)) {
                message("getting cached Inverse Matrix")
                return(iX)
        }
        iX <- solve(X)
        X$setInverse(iX)
        iX		
}
