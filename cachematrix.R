## Following are two functions which is used to cache a value of inverse of matrix if
## the matrix is the same matrix


## makeCacheMatrix: This function creates a special "matrix"
##object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
   ##setting the value of matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x  ##get value of matrix
  
  setInverse <- function(inv) m <<- inv    ##store the inverse for caching
  
  getInverse <- function() m       ##function to return the cached inverse
  
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve will
##  retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)    ##calculating Inverse
  x$setInverse(m)
  m
}
