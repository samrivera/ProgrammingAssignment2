## This function creates a special matrix object that can cache its
## inverse. The returned special matrix object will contain a list of
## 4 functions: set, get, setinverse, and getinverse.
makeCacheMatrix <- function(x = matrix()) {
        # assign the inverse matrix value to null
        invrs <- NULL
        # define function to set the matrix value
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        # define function for returning the matrix
        get <- function() x
        # define function for setting the inverse matrix value
        setinverse <- function(solve) invrs <<- solve
        # define function for getting the inverse matrix value
        getinverse <- function() invrs
        # return a list of these 4 functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of 'x' where 'x' is a special matrix
## object created by the function makeCacheMatrix(matrix). You must first
## call makeCacheMatrix(matrix) on your standard matrix object, then call
## this function.
cacheSolve <- function(x, ...) {
        # Check if the inverse matrix value is in the special matrix object
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                # The inverse matrix value was found, so just return it
                message("getting cached data")
                return(invrs)
        }
        
        # The inverse matrix was not found
        # Get the matrix value
        data <- x$get()
        
        # Call solve() on the matrix to get the inverse matrix
        invrs <- solve(data)
        
        # Set the inverse matrix in the cache and return the inverse matrix
        x$setinverse(invrs)
        invrs
}
