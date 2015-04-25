#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Caching the inverse of a matrix
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# function to return inverse of the matrix but if the inverse is
#   already calculated, then it retrives the inverse from cache

cacheSolve <- function(x, ...) {
  ## return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("retriving cached data") # message to console
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinv(m)
  m #return inverse
}