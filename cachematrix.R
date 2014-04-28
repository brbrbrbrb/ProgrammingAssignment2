##Programming Assignment 2 for R Programming, by Ben Bedore
##Two functions to create a cacheable matrix and invert that
##matrix either by calculation or (subsequently) through caching.

##makeCacheMatrix() creates a special matrix that caches matrix
##and matrix inverse values using local functions to get and set matrices.

makeCacheMatrix <- function(x = matrix()){
     m <- NULL
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##cacheSolve inverts a special matrix and returns the inverted matrix.
##Subsequent calls of cacheSolve for a given matrix return the cached
##version of that inverse matrix.

cacheSolve <- function(x,...){
     m <- x$getinverse()
     if(!is.null(m)){
          message("getting matrix inverse data")
          return(m)
     }
     data <- x$get()
     m <- solve(data,...)
     x$setinverse(m)
     m
}
