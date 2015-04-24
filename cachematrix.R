## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix composes the list that contains the function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mi <<- inverse
  getinverse <- function() mi
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
# This function returns the inverse of the matrix.If the computation of the inverse
# has been done, it skips it and returns the value from the cache.
#This function takes for granted that the matrix  is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting data from cache")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data)
  x$setinverse(mi)
  mi
}

## Testing
x <- matrix(1:4, 2,2); 
m <- makeCacheMatrix(x)
m$get()
###> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
cacheSolve(m)
##> cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(m)
##getting data from cache
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

