# Put comments here that give an overall description of what your
# functions do
#Assignment: Caching the Inverse of a Matrixless 
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

  
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#   This is same as make a vector function in example but with inverse instead of mean

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#For this assignment, assume that the matrix supplied is always invertible.

# Write a short comment describing this function  ( must change data to matrix)



makeCacheMatrix <- function(x = matrix()) {
 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


# Write a short comment describing this function
# same like Cachemean in example but with inverse

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
  
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
  }

B <- matrix(c(5,10,22,46,5,16),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)

x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()

x <-makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3))
cacheSolve(x)  

