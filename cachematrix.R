## Below listed functions cache the inverse of a matrix



## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr<<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) invr <<- inverse
  getInverse <- function() invr
  list (set= set, get= get, setInverse= setInverse, getInverse=getInverse)

}


## This function computes the inverse of special matrix returned by makeCachematrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr <- x$getInverse()
  if(!is.null(invr)) {
    
    message("getting chached data")
    return(invr)
  }
  mtrx <- x$get()
  invr <- solve(mtrx, ...)
  x$setInverse(invr)
  invr
}
