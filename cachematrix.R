##This function saves the matrix and the inverse (if exists) in the cache

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function pull the matrix from pullCacheMatrix function
## then return the inverse. If the inverse already exists, it doesnt
## compute again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
   if(!is.null(inv)) {
      message("getting cached matrix")
      return(inv)
   }
   mx <- x$get()
   inv <- solve(mx, ...)
   x$setinv(inv)
   inv
}
