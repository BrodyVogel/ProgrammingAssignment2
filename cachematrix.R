## These functions caches the inverse of a matrix, which could save time with huge matrices

### This function creates a process for caching the inverse of a matrix, if it exists
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) mat <<- inverse
  get_inverse <- function() mat
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


### This function either retrieves the cached inverse or calculates that inverse of the matrix
cacheSolve <- function(x, ...) {
  mat <- x$get_inverse()
  if (!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$set_inverse(mat)
  mat
}

### Test
#test <- matrix(c(1,2,3,4),2,2)
#test_1 <- makeCacheMatrix(test)
#cacheSolve(test_1) #inverse returned after computation
# this time returns the cached matrix
#cacheSolve(test_1) #inverse returned from cache
