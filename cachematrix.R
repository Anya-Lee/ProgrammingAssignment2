## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Functions that cache the inverse of a matrix
##
## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makecacheMatrix <- function (x = matrix()) {
  my_inv <- NULL
  set <- function (y) {
    x <<- y
    my_inv <<- NULL
  }
  get <- function () x
  setMyInverse <- function(inverse) my_inv <<- inverse
  getMyInverse <- function () my_inv
  list (set = set,
        get = get,
        setMyInverse = setMyInverse,
        getMyInverse = getMyInverse
        )
}

## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function (y, ...) {
  # get the cached value
  my_inv <- y$getMyInverse ()
    # if a cached value exists return itmy
  if (!is.null(my_inv)) {
      message("getting cached data")
      return(my_inv)
  }
  mat <- y$get()
  my_inv <- solve(mat)
  y$setMyInverse(my_inv)
  my_inv
}
