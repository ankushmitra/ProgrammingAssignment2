## Put comments here that give an overall description of what your
## functions do
#  makeCacheMatix:  Creates a special matrix object that caches its inverse
#  cacheSolve: Calcuates the inverse of the 'special matrix' created by 
#              makeCacheMatrix.          


## Write a short comment describing this function
# makeCacheMatrix: Create the special matrix object that can caches its inverse. 
#                  Used in conjunction with cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
        # inverse: the value that stores (caches) the inverse of matrix x
        inverse <- NULL
        
        # set: create the matrix to be chached
        set <- function(input_maxtrix) {
                x <<- input_matrix
                inverse <<- NULL                
        }
        
        # get: Return the cached matrix x
        get <- function() x
        
        #  setinverse: set the value of the inverse of matrix x
        setinverse <- function(inverse_of_matrix) {
                inverse <<- inverse_of_matrix
        }
        
        # getinverse: return the inverse of matrix x
        getinverse <- function() inverse
        
        list(set = set, get = get, 
             setinverse = setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# cacheSolve: Returns the inverse of input matrix 'x'.  If the inverse of 
# input matrix 'x' has been calculated, the cached result is returned, thereby
# avoiding reculculating the inverse more than once.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        # inverse is not null means inverse of 'x' has been calculated, so can
        # return cached result and avoid calcualting inverse again
        if (!is.null(inverse)) {
                message("Getting cached inverse")
                return(inverse)
        }
        
        # got here... that mean inverse is null, which means the inverse of 'x'
        # has not been calculated.  Calculate it now and cache the result for 
        # the future.
        message("Calculating inverse for the first time")
        cachedmatrix <- x$get()
        inverse <- solve(cachedmatrix,...)
        x$setinverse(inverse)
        
        # Finally return inverse of 'x' 
        inverse                        
}
