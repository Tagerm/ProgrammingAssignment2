## This function storage in cache the data and the function to get the inverse
## of a matrix.(this function only support squared matrixes)

makeCacheMatrix <- function(x = matrix()) {
ma <- NULL
set <- function(y) {
        x <<- y
        ma <<- NULL 
}
get <- function() x
setinverse <- function(solve) ma <<- solve
getinverse <<- function() ma 
list(set = set, get = get, 
     setinverse = setinverse, getinverse = getinverse)
}


## This function read the data in cache to figure out if there is the inverse
## of the matrix already calculated if its calculated will show the result. 
## if its no calculated yet then this function
## will calculate it and then show the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ma <- x$getinverse() 
        if(!is.null(ma)){
                message("Getting cached data")
                return(ma)
        }
        data <- x$get()
        ma <- solve (data, ...) 
        x$setinverse(ma)
        ma
}

poli <- matrix(c(2, 4, -3, -7) ,nrow = 2, ncol = 2)
cacheSolve(makeCacheMatrix(poli))

