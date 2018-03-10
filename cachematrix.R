##  Caching the Inverse of a Matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        new_matrix <-  NULL
        set <- function(y){
                x <<- y
                new_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_matrix){
                new_matrix <<- inverse_matrix
        }
        getinverse <- function() new_matrix
        list(set = set, get= get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        new_matrix <- x$getinverse()
        if(!is.null(new_matrix)){
                return(new_matrix)
        }
        new_matrix <- solve(x$get())
        x$setinverse(new_matrix)
        new_matrix
}
