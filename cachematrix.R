## The following pair of functions compute the inverse of a matrix object. If the inverse has been computed and the matrix has not changed, then the cache inverse of a matrix will be returned.

## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
  	set<-function(y) {
    	x <<- y
    	m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set, get=get, 
       setsolve=setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of a matrix returned by makeCacheMatrix above. If the inverse has been calculated and the matrix has not changed, then the cache inverse of a matrix will be returned.

cacheSolve <- function(x, ...) {
        m<- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data,...)
  x$setsolve(m)
  m
}
