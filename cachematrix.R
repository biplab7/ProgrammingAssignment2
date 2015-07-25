## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set <-function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    ##  Got Cached data
    #print("Got cached data")
    return(inv)
  }
  
  ##Didnot get Cached data, calculate the inverse
  
  inv1.data = x$get()
  inv=solve(inv1.data,...)
  x$setinverse(inv)
  return(inv)
}
