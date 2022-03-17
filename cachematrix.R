

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(matrix){
    x <<- matrix
    i <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse){
    i <<- inverse
  }
  getInverse <- function(){
    i
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  m
}

