## This checks to see if inverse of matrix has been cached, if not is cache's inverse of matrix
## example usages below
## b <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
## x1 <- makeCacheMatrix(b)
## cacheSolve(x1)

makeCacheMatrix <- function(x = matrix()) {
  #set inverse matrix to null for logical operation in other function
  mi <- NULL 
  #if we want to change the matrix
  set <- function(y){
    x <<- y
    mi <<- NULL
  }
  setinverse <- function(dat) mi <<- dat
  getmatrix <- function() x
  getinverse <- function() mi
  #makes functions callable
  list(getmatrix = getmatrix, getinverse = getinverse, 
       setinverse = setinverse, set = set)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached inverse")
    return(mi)
  }
  data <- x$getmatrix()
  mi <- solve(data)
  x$setinverse(mi)
  mi
}
