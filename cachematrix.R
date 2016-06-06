## makeCacheMatrix(): creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve(): computes the inverse of the "matrix" returned by 
## makeCacheMatrix(). If the inverse has already been calculated and 
## the matrix has not changed, it'll retrieves the inverse from the cache directly.

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) { 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Return a matrix that is the inverse of 'x'
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  #changesgetwd()
  x$setinv(inv)
  
  return(inv)
}
