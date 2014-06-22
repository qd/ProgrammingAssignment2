### makeCacheMatrix(x) sets up a placeholder list for
## the matrix variable x and the inverse matrix INV
##
## cacheSolve(x) returns the inverse INV or the cached INV
## if it already exists in the placeholder list
## see example, below:
##
## Caution: if the values of the matrix 'x' change
## makeCacheMatrix(x) needs to be called with the updated values
##
## ----  START of sample input/output of actual run----
## > A <- matrix(c(1,5,2,-5),2,2)  
## > A
## [,1] [,2]
## [1,]    1    2
## [2,]    5   -5
## > Acache <- makeCacheMatrix(A)
## > cacheSolve(Acache) 
## [,1]        [,2]              # the first time
## [1,] 0.3333333  0.13333333    # the inverse is calculated
## [2,] 0.3333333 -0.06666667
## > cacheSolve(Acache)
## getting cached data           # subsequent calls
## [,1]        [,2]              # return cached data
## [1,] 0.3333333  0.13333333
## [2,] 0.3333333 -0.06666667
## > 
## ----  END of sample input/output of actual run----
##
## makeCacheMatrix(x) sets up a placeholder list for
## the matrix variable x and the inverse matrix INV
makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) INV <<- inverse
  getinverse <- function() INV
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##
##  cacheSolve(x) computes the inverse of the special
## "matrix" list returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated, then
## `cacheSolve` retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  INV <- x$getinverse()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- solve(data, ...)
  x$setinverse(INV)
  INV
}
