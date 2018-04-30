## Functions to demonstrate use of lexical scoping R to provide caching mechanism for matrix inversion calculation

## function to creates a special "matrix" object that can cache its inverse.
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
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    
    ## return functions via list to preserve environment
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## compute inverse of CacheMatrix object or return cached result
cacheSolve <- function(x, ...)
{
    ## check to see if there is an existing cached result
    i <- x$getsolve()
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
        x$setsolve(i)
        i
    }
}
