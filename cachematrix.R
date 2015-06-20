## makeCacheMatrix and cacheSolve functions atteempt to find the inverse of a matrix and returns from cache
## if already available, else creates the inverted matrix and caches it for ready return witout computing it if
## called for the same matrix a second time


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initalizing matrixInverse to NULL. THis will store the matrix inverse
        matrixInverse<-NULL
        # Function to set the incoming matrix and reset the matrix inverse 
        setMatrix<-function (y=matrix()) {
                x <<- y
                matrixInverse<<- NULL
        }
        # Function to return the value of incoming matrix        
        getMatrix <- function() x
        # Function to caluclate and set the value of the inverted matrix in the cached object
        setInversion<- function (solveVal){
                matrixInverse <<-solveVal
        } 
        # Function to return the value of the inverted matrix
        getInversion <- function () matrixInverse
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,  setInversion = setInversion,
             getInversion = getInversion)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # Call the getInversion Function to return cached inverted matrix if available
        matrixInverse <- x$getInversion()
        print (matrixInverse)
        # Check if the Matrix is not empty, return inverted matrix
        if (!is.null(matrixInverse)){
                message("Retreiving matrix from cache")   
                return(matrixInverse)
        }
        #In case Matrix is empty
        message("No cached matrix")
        matrixData <- x$getMatrix()
        matrixInverse<-x$setInversion(solve(matrixData, ...))
        return(matrixInverse)
}

## Commands to run/test the functions
# Call makeCacheMatrix with a square matrix. This will set the incmoing square matrix and return the special vector
        #d<- makeCacheMatrix(matrix(2:5,2,2))
# Next Call CacheSolve with the special vector. First time it will set the inverse and return it. Second time it will retrieve from cache        
# cacheSolve(d)
        # Output first time
                # No cached matrix
                # [,1] [,2]
                # [1,] -2.5    2
                # [2,]  1.5   -1
        # Output on running  cacheSolve(d) again
                # Retreiving matrix from cache
                # [,1] [,2]
                # [1,] -2.5    2
                # [2,]  1.5   -1