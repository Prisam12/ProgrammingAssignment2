## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function is used to create a list of functions to set and get the value of a 
# matrix and also set and get the inverse of a matrix 
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
 
}


## Write a short comment describing this function

#The following function calculates the mean of the special "vector" created with the 
#above function. However, it first checks to see if the matrix has already been calculated. 
#If so, it gets the matrix from the cache and skips the computation. Otherwise, it calculates 
#the inverse of the data and sets the value of the matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
}
