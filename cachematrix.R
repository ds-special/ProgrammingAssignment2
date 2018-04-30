## Functions to demonstrate use of lexical scoping R to provide caching mechanism for matrix inversion calculation

## function to creates a special "matrix" object that can cache its inverse.
## NOTE: only works with a square invertible matrix
makeCacheMatrix <- function(x = matrix())
{
    ## i holds reference to matrix inversion result
    i <- NULL
    
    ## set and get underlying matrix object
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    ## functions to get or set cached result
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    
    ## return functions via list to preserve environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## compute inverse of CacheMatrix object or return cached result
cacheSolve <- function(x, ...)
{
    ## check to see if there is an existing cached result
    i <- x$getinverse()
    if(!is.null(i))
    {
        message("getting cached data")
        return(i)
    }
    else
    {
        ## calculate, cache and return inverse of matrix
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
    }
}
