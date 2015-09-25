## makecacheMatrix function is the assignment function and in the end creates a list of functions
## there are 4 functions set,get,setInverse,getInverse
## cachesolve function checks if the inverse is already calculated or it needs to be calculated again.


## Assignement function returns cached inverse matrix data

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## checks if Inverse is cached or needs to be calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data for Matrix Inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
