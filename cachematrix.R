## The following functions initialize a set of functions used in 
## calculating the inverse of a matrix and caching the result as 
## well as peforming the inverse calculation after checking to see
## if the answer is already in the cache.

## makeCacheMatrix initializes a list of functions 
## with an input matrix and clears the cache.
## To use this funtion pass a matrix to it and assign it to a variable


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## This function takes the list of initialized functions 
## It checks the cache and if it is empty calculates the
## inverse of the matrix used to initialize the functions.
## To use this function pass it the variable that the makeCacheMatrix function was assigned to

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
  
}
