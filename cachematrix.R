##Stores the list of input functions (setinverse, getinverse)

makeCacheMatrix <- function(x=numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##Checks to see if inverse of matrix has already been computed, if so
##returns message saying "getting cached data". If not it computes the inverse 
##(using solve) and caches it 

cachesolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
