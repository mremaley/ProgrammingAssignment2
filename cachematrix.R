## Pair of functions that cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## Set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Get/Return the matrix
        get <- function() {
                x
        }
        
        ## Set inverse of matrix
        setInv <- function(inverse) {
                i <<- inverse
        }
        
        ## Get/Return inverse of matrix
        getInv <- function() {
                i
        }
        
        ## Return list containing functions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## Computes the inverse of the special "matrix" returned by the function above.
## If the inverse has already been calcuated (and the matrix is unchanged),
## cache should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        
        ## If inverse has been calculated, get inverse and skip computation
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Get matrix
        data <- x$get()
        
        ## Calculate inverse of data using "solve" function
        m <- solve(data) %*% data
        
        ## Set value of inverse in cache via "setInv" function & return matrix
        x$setInv(m)
        m
}
