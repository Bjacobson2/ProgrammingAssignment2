##Push V.2 -edited
##Functions to create, cache, and get the inverse of a matrix; goal to save time.

##The first function makeCacheMatrix creates a matrix, to set the value of the matrix and 'get' it,
##and set the value of the inverse, and get it. 
## The second function cacheSolve- Creates the inverse of the matrix, but first checks if already calculated
#If cached already, skips computation and gets value. Else, it is computed and cached.


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){x}
  setInverse <- function(solve){ m <<- solve}
  getInverse <- function() {m}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#cacheSolve: This function checks if the inverse of the matrix has been computed and gets it, otherwise will compute it

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

#in order to run, use sample<-makeCacheMatrix(matrix(...,..,..)), then cacheSolve(sample).
#possible error lapback routine if det(sample)=0, i.e. non-invertible