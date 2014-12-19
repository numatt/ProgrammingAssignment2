## These methods provide the ability to calculate the inverse of a matrix, and
## cache the result for re-use. The inverse is calculated the first time it is
## needed, and then the cached result is used thereafter.

## This function provides a list of functions which wrap a vector. They enable
## the capability to store (cache) the inverse of the vector.
##
## Arguments:
##      x                       matrix to wrap (defaults to empty matrix)
##
## The methods returned in the list are:
##      set(y)                  set the matrix
##      get()                   get the matrix
##      setinverse(inverse)     set the matrix's (cached) inverse
##      getinverse()            get the matrix's (cached) inverse
makeCacheMatrix <- function(x = matrix()) {
    m <<- x
    m_inverse <- NULL
    
    set <- function(y) {
        m <<- y
        m_inverse <<- NULL
    }
    
    get <- function() m
    
    setinverse <- function(inverse) m_inverse <<- inverse
    
    getinverse <- function() m_inverse
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function provides the inverse of a cached matrix. If the inverse is
## already cached, it returns the cached version. If it is not cached, it 
## calculates the inverse and caches it before returning the result. It also
## prints out a message indicating if the cached value was available.
##
## Arguments:
##      x                       cached matrix created by makeCacheMatrix
##      ...                     further arguments passed to solve
##
## Value:
##      The inverse of the cached matrix, provided from cache if possible

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    message("populating cache")
    
    inverse
}
