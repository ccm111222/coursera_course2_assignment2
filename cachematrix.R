## Put comments here that give an overall description of what your
## functions do

## A function that can set the matrix, get the matrix, set the inverse matrix,
## and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <- NULL
  }
  get <- function() {x}
  setInver <- function(z) {inver<<-z}
  getInver <- function() {inver}
  list(set=set, get=get, setInver=setInver, getInver=getInver)
}


## A function to get the inverse of the matrix either from cache or from calculation.

cacheSolve <- function(x, ...) {
  inver <- x$getInver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  } else {
    data <- x$get()
    inver <- solve(data,...)
    x$setInver(inver)
    inver
  }
}

## Some examples of the code
aaa<-makeCacheMatrix(matrix(1:4,ncol=2,nrow=2))
aaa$get()
aaa$getInver()
cacheSolve(aaa)
aaa$getInver()
