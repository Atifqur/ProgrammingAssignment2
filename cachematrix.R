# creates a special Matrix, which contains a function to
# set the value of the matrix and get the value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y){
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix)
  v <<- solveMatrix
  getInverse <- function() v
  list(set = set, get = get, setInverse = setInverse,
  getInverse = getInverse)
}

  ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  v <- x$getInverse()
  if(!is.null(v)){
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data)
  x$setInverse(v)
  v
}

m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
