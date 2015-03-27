## Amanda Molling 03/18/2015

## 

makeCacheMatrix <- function(x = matrix()) {
  ##creates a special matrix object that can cache its inverse
  m <- NULL
  set <- function(y){
    x<<- y
    m <<- NULL 
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}




cacheSolve <- function(x, ...)  { 
        ## Return a matrix that is the inverse of 'x'
  

  m <- x$getInverse()
  if( !is.null(m) )## check to see if it exists)
  {
    message("Recover cached data")
    return(m)
  }
  
  else
  {
    message("I have to compute the inverse")
  }
  data <- x$get()
  m<- solve(data)
  x$setInverse(m)
  m
  
}
