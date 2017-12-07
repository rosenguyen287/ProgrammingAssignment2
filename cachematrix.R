
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 

          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) { 
	## @x: output of cacheSolve()
        ## return: inverse of the original matrix input to cacheSolve()

  i <- x$getinverse()
     # if the inverse has already been calculated

  if (!is.null(i)) {
  	# get it from the cache and skips the computation. 

          message("getting cached data")
          return(i)
  }
        # otherwise, calculates the inverse 

  data <- x$get()
  i <- solve(data, ...)
   # sets the value of the inverse in the cache via the setinv function.

  x$setinverse(i)
  i
}