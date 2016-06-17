# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it 
# repeatedly (there are also alternatives to matrix inversion that 
# we will not discuss here). Your assignment is to write a pair of 
# functions that cache the inverse of a matrix.

## Write a short comment describing this function
# set(): set the value of the matrix
# get(): get the value of the matrix
# setinverse(): set the value of the inverse
# getinverse(): get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get<- function() x
  setinverse <- function(inverse) inv <<-inverse
  getinverse <- function() inv
  list(set=set, get=get, 
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
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


#example :
# > source('D:/LEARN/coursera/RProgramming/ProgrammingAssignment2/cachematrix.R')
# > mat <- makeCacheMatrix()
# > mat$set(matrix(rnorm(9), nrow = 3))
# > mat$get()
# [,1]       [,2]       [,3]
# [1,] 0.009154377  0.6205645  0.3114179
# [2,] 1.025674797  0.4395419  0.1698079
# [3,] 1.486203107 -1.4028245 -0.1498454
# > cacheSolve(mat)
# [,1]       [,2]        [,3]
# [1,] -0.4330889  0.8641202  0.07916705
# [2,] -1.0203860  1.1664857 -0.79874280
# [3,]  5.2571797 -2.3498651  1.58933271
# > #second time
#   > cacheSolve(mat)
# getting cached data
# [,1]       [,2]        [,3]
# [1,] -0.4330889  0.8641202  0.07916705
# [2,] -1.0203860  1.1664857 -0.79874280
# [3,]  5.2571797 -2.3498651  1.58933271

# Second Example
# > mat$set(matrix(rnorm(4), nrow=2))
# > mat$get()
# [,1]      [,2]
# [1,]  0.8934147 -1.399039
# [2,] -0.4176807 -0.307201
# > #first time
#   > cacheSolve(mat)
# [,1]      [,2]
# [1,]  0.3577057 -1.629045
# [2,] -0.4863485 -1.040295
# > #second time
#   > cacheSolve(mat)
# getting cached data
# [,1]      [,2]
# [1,]  0.3577057 -1.629045
# [2,] -0.4863485 -1.040295

