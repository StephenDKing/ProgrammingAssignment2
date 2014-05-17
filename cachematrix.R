## makeCacheMatrix: Creates a Cache of a Matrix
## cacheSolve: uses the created cache or builds the cache then returns it

## Function the gets and sets the matrix and its inverse, hold it in cache
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  
  #additional methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Checks to see if cache exists then returns it, otherwise makes the cache and then return it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix <- x$getInverse() # get inverse 
  if(!is.null(matrix)) {
    return(matrix) # return it if not null
  }
  data <- x$get()
  matrix <- solve(data, ...)
  x$setInverse(matrix)
  matrix
}

## to test 
## c <- replicate(100, rnorm(100)) 
## xf <- makeCacheMatrix(c) ##make the Cache
## cacheSolve(xf)
