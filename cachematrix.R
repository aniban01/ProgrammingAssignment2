
## this function returns a list that has certain functions 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } ## sets inverse
  get <- function() x  ## returns the element
  setinverse <- function(inverse) inv <<- inverse  ## sets inverse into the cache
  getinverse <- function() inv ## returns inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this calculates the inverse or checks whether the cache exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(identical(x$get, x))
        {
    
          if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    
        }
        else 
        {
          m <- solve{x}
          x$set(x)
          x$setinverse(m)
        }
}
