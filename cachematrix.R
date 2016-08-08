## There are two functions in this program. The first function will createa special "matrix" object
## That is actually a list with functions that store, calculate and return the inverse of the matrix
## The second function will help calculate or retrieve the inverse of the matrix

##This functionscreates a special matrix that can caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(matrix) i <<- solve
  getInverseMatrix <- function() i
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function caculates the inverse of the special matrix created from the previous function
## If the inverse has already caculated, the function will return the inverse from the cache
## otherwise, it will caculates the inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverseMatrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverseMatrix(i)
  i
}