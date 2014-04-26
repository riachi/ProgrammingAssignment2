## functions will set values for matrix and inverse matrix and allow 
## for to cache matrix values, inverse matrix values, and to 
## compute inverse matrix value if none is found cached,
## purpose behind this is to decrease computing time for inverse matrices
## which have already been cached

## makeCacheMatrix will create a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
        
        set <- function(y) {    ##  First Function sets 
                x <<- y         ##  the value of the matrix
                m <<- NULL
        }
        
        get <- function() {      ##  Second Function returns(gets) 
                x                ##  the value of the matrix
        }
        
        setInverse <- function(Inverse) {  ##  Third Function sets the 
                m <<- Inverse              ##  value of the inverse of the matrix
        }
        
        getInverse <- function() {   ##  Fourth Function returns(gets) 
                m                    ##  the cached value of the inverse of the matrix
        }
        
        list(set = set, get = get,    ##  assmebles functions into a list
             setInverse = setInverse,
             getInverse = getInverse)


}


## cacheSolve function will first check if there is a cached value for the matrix.
## If there is it will return this value, If there isnt it will calculate the inverse
## and return that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

     
        m <- x$getInverse()  ## query the x matrix's cache
        
        if(!is.null(m)) {    ## if there is a cache
                message("getting cached data")
                return(m)    ## return the cache, 
        }
        
                                 ## if there's no cache
        data <- x$get()
        library(MASS)           ## get library "MASS" for ginv invert matrix function
        m <- ginv(data, ...)   ## compute matrix inverse
        
        x$setInverse(m)         ## save the result back to x's cache
        m        ## return the result
                        
}
