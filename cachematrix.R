## 
## TEST TO GENERATE 2nd SHA Code

## Creates a matrix and its Inverse (stored in Cache)
## Returs a list of pointers to the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

        #INITIALIZE MATRIX
        
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(solve) inverse <<- solve
        
        getInverse <- function() inverse

        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function checks if inverse of a matrix is already availble in the Cache, if not 
## calculates and returns/prints the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getInverse()
        
        if (!is.null(inverse)) {
                message("Getting cached data")
                return
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse (inverse)
        inverse
}
