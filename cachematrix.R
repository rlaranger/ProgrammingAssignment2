## This function makes a matrix object that can cache its inverse. 
## makeCacheMatrix interacts with cacheSolve to return an inverted matrix
## makeCacheMatrix supports;
##  1) setting the value of a matrix
##  2) getting the value of a matrix
##  3) setting the value of the inverse of a matrix
##  4) getting the value of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes a matrix object made with makeCachematrix and gets the inverse
## This function gets the inverse using the solve() function with data as the argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get() #Returns the cached matrix as "data"
  inv <- solve(data) #This is where the inversion of the matrix is solved
  x$setinv(inv)
  inv
}



