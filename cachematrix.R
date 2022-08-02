## In order to overcome the computational cost associated with matrix inversion,
## the functions below are aimed at catching inverse matrices such that if the same
## matrix is loaded then the catched inversion of the matrix is loaded rather than
## performing one


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.invmat <- function(mat) m <<- mat
  get.invmat <- function() m
  list(set = set, get = get,
       set.invmat = set.invmat,
       get.invmat = get.invmat)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get.invmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.invmat(m)
  m
}
