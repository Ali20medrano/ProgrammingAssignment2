## This function creates a special "matrix" object that can cache its inverse.
## First I set the input "x" as a matrix
## then set the value "s" as a NULL
## Finally I change the function "mean" to "solve"


makeCacheMatrix <- function(x = matrix(1:12, nrow = 3, ncol = 4)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinversee = setinverse,
       getinverse = getinverse)
}


## Return the Matrix inverse of an invertible matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inversed matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


