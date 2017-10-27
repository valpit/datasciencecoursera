## Programming Assignement 2 : Lexical scoping

## This function creates a matrix that can catch its inverse
## matrix is inverted using tcahe function solve. Matrix should be square
## and assumed to be invertible

makeCacheMatrix <- function(matx = matrix()) {
  matx_inv <- NULL
  set <- function(maty) {
    matx <<- maty
    matx_inv <<- NULL
  }
  get <- function() matx
  setsolve <- function(solve) matx_inv <<- solve
  getsolve <- function() matx_inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function comptus the inverse of the matrix returned by the
## nakeCacheMatrix function
## if inverse matrix already been calculated, then cachesolve
## retrieves the inverse from the cache 

cacheSolve <- function(matx, ...) {
  matx_inv <- matx$getsolve()
  if(!is.null(matx_inv)) {
    message("getting cached data")
    return(matx_inv)
  }
  data <- matx$get()
  matx_inv <- solve(data, ...)
  matx$setsolve(matx_inv)
  matx_inv
}