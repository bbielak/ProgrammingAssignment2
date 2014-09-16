## Benjamin Bielak Jr | R Programming | Assignment 2
## Caching the Inverse of a Matrix

## makeCacheMatrix | Makes a special "matrix" which is really a list containing a function to:
##   (1) set the value of the matrix
##   (2) get the value of the matrix
##   (3) set the value of the inverse matrix
##   (4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve | Calculates the inverse matrix of the "matrix" created in makeCacheMatrix.  If it has
## already been calculated it will return the cached value, otherwise it will calculate the inverse
## matrix and cache it using the setsolve function
## NOTE: assumption that the matrix is invertable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

## ## test
## in_data <- matrix(c(4, 3, 3, 2), 2, 2)
## in_data
## [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## mcm <- makeCacheMatrix(in_data)
## cacheSolve(mcm)
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## cacheSolve(mcm)
## getting cached data
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
