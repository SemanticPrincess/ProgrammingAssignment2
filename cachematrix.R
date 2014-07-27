## overall:  these functions create a matrix and computes the inverse of the matrix as well as caches it 
# for future retrieval.  

## this function takes in a matrix and returns a list of functions. 
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setmatrix <- function(solve) m <<- solve   
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}



## this functions returns a matrix that is the inverse of 'x'.  
#The inverse of the matrix will be retrieved from cache if already created and gets created if not in cache.
cacheSolve <- function(x, ...) 
{
        m <- x$getmatrix()

        if(!is.null(m)) 
        {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

