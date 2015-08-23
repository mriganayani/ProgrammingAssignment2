## Put comments here that give an overall description of what your
## functions do
#Function does the following
#create a matrix and store
#calculate the inverse of the matrix and store it
# get the stored matrix inverse (if not already calculated)

## This function creates the matrix, inverts it and caches matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


## This function computes the inverse of the matrix that is returned above 
# IF it hasn't been computed already. If so, it just gets it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
