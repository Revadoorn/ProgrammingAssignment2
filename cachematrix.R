# Make a cache matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  set <- function (y)
  {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinv <- function(inv) m <<- inverse
  getinv <- function() m
  list ( set = set, get = get, setinv = setinv, getinv = getinv)
  
}

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  # if already been calculated
  if (!is.null(m))
  {
    # get form cache
    message ("getting cached data")
    return (m)
  }
  
  #otherwise calculate
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
