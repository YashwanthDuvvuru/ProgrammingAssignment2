## In this assignment, we are trying to calculate and cache
## the inverse of a given matrix

## makeCacheMatrix returns a list of 4 functions -
## (set, get, setinverse, getinverse)
## It helps in caching a matrix, its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    cachedInverse <- NULL
    
    ## caches a matrix
    set <- function(matrix)
    {
        x <<- matrix
        cachedInverse <- NULL
    }
    
    ## gets cached matrix
    get <- function() x
    
    ## caches inverse of a matrix
    setinverse <- function(inverse)
    {
        cachedInverse <<- inverse
    }
    
    ## gets cached inverse
    getinverse <- function() cachedInverse
    
    ## returns list of 4 functions
    list(set = set, 
         get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve returns inverse of a matrix
## It calculates inverse if not already calculated
## otherwise cached inverse is returned

cacheSolve <- function(x, ...) 
{
    inverse <- x$getinverse()
    matrix <- x$get()
    
    ## checks if inverse is previuosly cached or not
    if(!is.null(inverse))
    {
        ## returns cached inverse
        message("Getting cached inverse")
        return(inverse)
    }
    
    ## calculates inverse of a matrix
    inverse <- solve(matrix)
    ## caches inverse of a matrix
    x$setinverse(inverse)
    
    ## returns calculated inverse
    inverse
}
