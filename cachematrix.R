## Put comments here that give an overall description of what your
## functions do

## this function makes a new "special matrix" object

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(my.inv) m <<- my.inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##calculate the inverse of a given matrix (or "special matrix")
## if a "special matrix" is given for which inverse has been already calculated,
## then alreadmy in-memory result is returned with a message "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
