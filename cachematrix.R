## Harrison Hassig - Sept 22, 2016
## This set of functions saves time in computing inverses of
## matrices by caching the inverse as opposed to computing the inverse repeatedly. 

## makeCacheMatrix:
## A list containing a function to:
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse of matrix
## 4 - get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }


## cacheSolve:
## Returns inverse of matrix by:
## 1 - check if inverse has been computed
## 2 - if not, computes inverse
## 3 - sets inverse in cache via "setinverse"

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
