##  A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It takes a an empty matrix as argument

makeCacheMatrix <- function(x = matrix()) {
  
      #initialize the value of the matrix inverse to NULL
      matrixInv <- NULL
      set <- function(y){
            x <<- y
            matrixInv <<- NULL
      }
      
      # gets the value of the inverse
      get <- function() x
      
      #calculates the inverse of matrix via the solve function
      setInverse <- function(solve) matrixInv <<- solve
      
      # gets the inverse
      getInverse <- function() matrixInv
      
      # passes the value of the function makeCacheMatrix 
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been computed, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInv <- x$getInverse()
        
        #if the inverse exists in cache, it gets it.
            if(!is.null(matrixInv)){
              message("getting cached data")
              return(matrixInv)
            }
        
        #if the inverse if not there, first it is calculated and then retrieved.
        data <- x$get()
        matrixInv <- solve(data,...)
        x$setInverse(matrixInv)
        matrixInv
}
