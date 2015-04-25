## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix defines four functions related with inverse matrix as follows
# set: to set the source matrix
# get: to get the source matrix
# setInverse: to set the inverse of the matrix
# getInverse: to get the inverse of the matrix
# set function also checks for valid matrix and returns appropriate messages if
# 1. a non-matrix is sent, it is not stored as we can't calculate the inverse
# 2. a null matrix is sent, is is not stored as we can't calculate the inverse
# 3. same matrix is sent again, no further processing is done
# 4. Any other case is treated as valid and the matrix is stored internally
makeCacheMatrix <- function(sourceMatrix = matrix()) {
        inverseMatrix ={} # holds the value of inverse matrix calculation in cache
        # sets the source matrix
        set <- function(newMatrix) 
        {# conditions to check the valid input
                if(!is.matrix(newMatrix)) message ("Invalid input matrix")
                else if (is.null(newMatrix)) message ("Null input matrix") 
                else if (identical(sourceMatrix, newMatrix)) message ("Input already set")
                else
                {                     
                        sourceMatrix <<- newMatrix
                        inverseMatrix <<- NULL
                }
        }
        #returns the source matrix
        get <- function() sourceMatrix
        
        #stores the inverse of a matrix
        setInverse <- function(newInverseMatrix) inverseMatrix <<- newInverseMatrix
        
        #returns the inverse matrix
        getInverse <- function() inverseMatrix
        
        #returns the list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve function is used to calculate and return the inverse of the matrix. It receives an
# input list created by makeCacheMatrix function
# It handles error condition for the case where matrix set in makeCacheMatrix is not a square matrix
# Else if the matrix is a square matrix, cacheSolve returns a valid inverse matrix.
# inverse matrix is calculated only the first time call for a newly set matrix. Subsequent calls
# to cacheSolve for same set matrix returns the result from cache instead of recalculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get the Inverse matrix. It is NULL if called first time and non-NULL for subsequent calls
        inverseMatrix <- x$getInverse()
        # If the inverse matrix returns is not NULL, return this value coming from cache
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        # Else if the inverse returned was NULL, get the source matrix and find its inverse
        sourceMatrix <- x$get()
        # check if the matrix is square, return error if not
        if (nrow(sourceMatrix) == ncol(sourceMatrix)) 
        {       
        # find the inverse of matrix
         inverseMatrix <- solve(sourceMatrix)
         # set the inverse matrix in cache
         x$setInverse(inverseMatrix)
         inverseMatrix
        } else 
                message("Input not invertible")
}
