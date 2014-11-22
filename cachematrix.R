## makeCacheMatrix and cacheSolve are used to do matrix inversion and speed up 
## the further calculations by storing the inverse matrix in a cache.

## The makeCacheMatrix function takes a matrix as input and returns a list of 
## functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix (and cache the value)
## 4. get the value of the inverse of the matrix
## Note: The matrix is assumed to be a square invertible matrix (not all matrices
## are invertible; check matrix if you get an error message).

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse matrix as NULL
        m <- NULL
        
        ## 1. Defines 'set' to set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## 2. Defines 'get' to get the value of the matrix
        get <- function() x
       
        ## 3. Defines 'setinverse' to set the value of the inverse matrix
        setinverse <- function(inverse) m <<- inverse
        
        ## 4. Defines 'getinverse' to get the value of the matrix
        getinverse <- function() m
       
        ## returns a list containing the arguments defined above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function returns a matrix that is the inverse of 'x'.
## It will first check if the inverse has been calculated already.
## If it was already calculated it will get it from the cache.
## otherwise it calculates the inverse of the data and sets the value 
## of inverse in the cache

cacheSolve <- function(x, ...) {
        ## get inverse from the list created in makeCacheMatrix
        m <- x$getinverse()
        
        ## check if the inverse has been calculated. if yes, return value of
        ## from the cache (plus display message)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If the inverse has not been calculated,
        ## then get the value of the matrix using get function
        data <- x$get()
        
        ## calculate the inverse using solve
        m <- solve(data, ...)
        
        ## set the inverse using setinv function
        x$setinverse(m)
        
        ## return the value of the inverse
        m
}


