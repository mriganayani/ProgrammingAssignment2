## Put comments here that give an overall description of what your
## functions do
#Function does the following
#create a matrix and store
#calculate the inverse of the matrix and store it
# get the stored matrix inverse (if not already calculated)

## This function creates the matrix, inverts it and caches matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

  invMat <- NULL
  
  # Define the data, and all functions and create a new invMat in global env
  set <- function(m) {
    mat <<- m
    invMat <<- NULL
  }
  set(x)
  
  get <- function() {return(mat)}
  
  setInv <- function(iM) {invMat <<- iM}
  
  getInv <- function() {return(invMat)}
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}
  
## This function computes the inverse of the matrix that is returned above 
# IF it hasn't been computed already. If so, it just gets it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInv()
  
  # Pull the inverse from cache if it's there
  if(!is.null(m)) {
    cat("Getting cached data.\n")
    return(m)
  }
  # Otherwise, calculate the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  return(m)
}
