## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# set function caches the matrix passed through  makeCacheMatrix
# get function returns the cached Matrix
# SetInvMatrix function caches the inverse matrix passed to it
# getInvMatrix returns the cached inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInvMatrix <- function(invMatrix) inv <<- invMatrix
  
  getInvMatrix <- function() inv
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix =getInvMatrix)
}


## Write a short comment describing this function
# it checks whether inverse matrix can be taken from earlier computed and stored cache if not 
# will compute the inverse for teh cached Matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInvMatrix()  
  if (!is.null(inv)) {
    message("getting cached data - inverse matrix")
    return(inv)
  }
  mat1 <- x$get()
  inv <- solve(mat1, ...)
  
  x$setInvMatrix(inv)
  inv
}

