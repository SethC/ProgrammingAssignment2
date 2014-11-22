## Functions to calculate the inverse of a square matrix
## using cached values when available.

## creates get and set functions for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    #m default as null
    m <- NULL
    
    #set function to store matrix
    set <- function(y) {
        x<<-y
        m<<-NULL
    }
    
    #get function to get matrix
    get <- function() x
    
    #set matrix inverse function
    setinverse <- function(solve) m <<- solve
    
    #get cached matrix inverse
    getinverse <- function() m
    
    #list to store 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## returns the inverse of matrix using cached value if available

cacheSolve <- function(x, ...) {
    
    #set m to value of inverse of x if already stored in cache, null otherwise
    m <- x$getinverse()
    
    #check value of m, if not null get cached value of inverse matrix
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    #if value of m is null then continue to calc and store matrix inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    #return newly calculated value of matrix inverse
    m
}
