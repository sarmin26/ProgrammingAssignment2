## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makecacheMatrix consists of set, get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                     ## initializing inverse as NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x           ## function to get matrix x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i    ## function to obtain inverse of the matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## write a short comment describing this function
#### This is used to get the cache data
cacheSolve <- function(x, ...) {    ## gets cache data
  i <- x$getinverse()
  if(!is.null(i)) {                 ## checking whether inverse is NULL
    message("getting cached data")
    return(i)                       ## return inverse value
  }
  data <- x$get()
  i <- solve(data, ...)             ## calculates inverse value
  x$setinverse(i)
  i                                 ## return a matrix that is the inverse of 'x'
}
