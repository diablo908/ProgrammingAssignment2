## The following functions together compute and cache the inverse of an 
## invertable square matrix 

## The following function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cachem <- NULL
        set <- function(y) {
                x <<- y
                cachem <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) cachem <<- solve
        getinverse <- function() cachem
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function returns a matrix that is the inverse of 
## a given square matrix. The function first checks to see if the 
## inverse has been calculated. If it has, it returns the cached 
## result. If it has not, it calculates and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}