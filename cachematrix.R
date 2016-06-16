# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it 
# repeatedly (there are also alternatives to matrix inversion that 
# we will not discuss here). Your assignment is to write a pair of 
# functions that cache the inverse of a matrix.

## Write a short comment describing this function
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get<- function() x
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function() inv
  list(set=set,get=get, 
       setinverse = setinverse, 
       getinverse =getinverse)
  
}


# The following function computes the inverse of the matrix. 
# It first checks if the inverse has alrady been computed. If so, 
# it returns the result from the cache, otherwise it computes the inverse,
# and caches it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data<- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
