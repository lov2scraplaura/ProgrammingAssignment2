## Put comments here that give an overall description of what your
## functions do
## 
## The functions include:
##   set  changes the value of a matrix to the argument in the main function
##   get  has no argument and returns the matrix stored in the main function
##   setinverse  stores the value of the calculated inverse from the main function 
##   getinverse  returns the value of the calculated inverse from the main function
##
## Write a short comment describing this function.
##  The makeCacheMatrix is a function that defines 4 different functions to be called
##  in another function.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL  ## 
     set <- function(y){
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) i <<- solve
     getinverse <- function() i
     return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}
## Write a short comment describing this function
##
## this function checks to see if this inverse has been stored in cache
## if it has been stored, then it gets the cached data.
##
## if it has NOT been stored, then it calculates the inverse and returns the inverse
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if (!is.null(i))  {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
