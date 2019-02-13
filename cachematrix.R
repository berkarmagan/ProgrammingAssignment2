## makeCacheMatrix is a func and needs a matrix to evaluate. It has basicly 2 func; setinverse and getinverse. Setinverse will use in the
## cacheSolve func if it s the 1st time for trying to get inverse for that session. If it s not the first time then getinverse will use 
## in the cacheSolve which stores the already inversed data.

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y) {x <<- y
    t <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) t <<- inverse
  getinverse <- function() t
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  t <- x$getinverse()
  if (!is.null(t)) {message("getting cached data")
    return(t)
  }
  dataset <- x$get()
  t <- solve(dataset, ...)
  x$setinverse(t)
  t
}



