## The function makeCacheMatrix create a matrix object that can cache its inverse
## The function cacheSolve computes the inverse of the matriz and cache the result (the first time only)
## The second time is executed cacheSolve gets the cached result so is fast and optimal
## Tested with this example:
##
## c=rbind(c(1, -1/4), c(-1/4, 1))
## solve(c)
##        [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##
## d<-makeCacheMatrix(c)
## cacheSolve(d)
##        [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## 
## cacheSolve(d)
## 
## getting cached data
##      [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## So cacheSolve(d) = Solve(c) but getting the value from cache since the first time is calculated 



## The function makeCacheMatrix create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## The function cacheSolve computes the inverse of the matriz and cache the result (the first time only)
## The second time is executed cacheSolve gets the cached result so is fast and optimal
## the message "getting cached data" shows that the inverse has been retrieved from cache and not calculated again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

