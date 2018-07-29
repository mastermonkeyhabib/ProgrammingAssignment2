## This pair of methods is able to calulate the inverse of a matrix, and store that matrix with it's inverse matrix
## makeCacheMatrix provides utility functions for storing and retreiving matixes in cache
## cacheSolve computes the inverse matrix and manages the matrixes in cache
##
## sample calls:
##   data = matrix(c(2,2,3,2), ncol = 2, nrow = 2)   ## Create an invertible matrix
##   data2 = makeCacheMatrix(data)                   ## Special matrix object storing matrix
##   cacheSolve(data2)                               ## Get the inverse matrix, first time, calculate, store in cache, return
##   cacheSolve(data2)                               ## Get the inverse matrix, this time from cache
##   data = matrix(c(2,3,3,2), ncol = 2, nrow = 2)   ## New matrix to work with
##   data2$set(data)                                 ## Set the new matrix
##   cacheSolve(data2)                               ## Get the inverse matrix, first time, calculate, store in cache, return
##   cacheSolve(data2)                               ## Get the inverse matrix, this time from cache


## Function provides caching abilities for matrix.  get and set store and retreive matrix of data.  setinversematrix and getinversematrix store and retreive inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL ## This is the inverse matrix stored in cache, starts as NULL
     set <- function(y) {
          x <<- y ## store incoming data into X.  
          m <<- NULL  ## NULL out the inverse
     }
     get <- function() x ##Simply return the current invers array, may be null
     setinversematrix <- function(mtx) m <<- mtx
     getinversematrix <- function() m
     list(set = set, get = get,
          setinversematrix = setinversematrix,
          getinversematrix = getinversematrix)
}


## Function will calculate the inverse of a matrix.  If matrix exists in cache, cached matrix is used.  Otherwise calculate and put in cache.

cacheSolve <- function(x, ...) {
     m <- x$getinversematrix()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     message("not from cache, calculate inverse")
     data <- x$get()
     m <- solve(data) ## calculates inverse
     x$setinversematrix(m) 
     m
}



