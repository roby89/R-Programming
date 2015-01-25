# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 
# The output is a simple fist of functions: get, setinv, and getinv

makeCacheMatrix <- function(x=matrix()) {
  x<<-x
  inv_x <<- NULL
  get <- function() x
  setinv <- function(inv) inv_x <<- inv
  getinv <- function() inv_x
  list(get=get,setinv=setinv,getinv=getinv)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve
# the inverse from the cache.


cacheSolve <- function(x, ...) {
  inv <- x$getinv()

  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

#
# Test Matrix
#

 X<-matrix(c(1,-1,0,2,0,0,3,1,-8),3,3)
 solve(X)

 output<-makeCacheMatrix(X)
 cacheSolve(output)
