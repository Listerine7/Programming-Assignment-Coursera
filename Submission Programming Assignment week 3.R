## The function "makeCacheMatrix" takes an invertible matrix
## and transforms it into a list that is usable by the "cacheSolve" function
## Always assign the output of the "makeCacheMatrix" to a variable to work further with it as it does not transform the input matrix itself


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## "cacheSolve" takes a list produced by the "makeCacheMatrix" function and 
## inverts the initial matrix "makeCacheMatrix" used as input. The output is a matrix again. 
## This function cannot use a matrix as input directly

  cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
  message("getting cached data")
  print(inv)
  }
  else {data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  print(inv)}
  }
  