
#Below are two functions that are used to create a special object that stores a matrix and cache's its inverse

#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(x = matrix(rnorm(100,mean=10, sd=3), ncol= 10), nrow=10) { #create a matrix with 100 elements (placed in 5 columns, 20 rows) comes from N(10,3)
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #solve function used for getting inverse of matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The following function calculates the inverse of the special matrix created with the above function
# If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
