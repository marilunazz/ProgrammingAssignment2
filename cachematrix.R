## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" which is really a list containing
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- solve(x)
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv, getinv = getinv)
}


## It calculates the inverse of the special matrix but it first checks
## to see if the inverse has already been calculated. 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      mtrx <- x$get()
      m <- solve(mtrx)
      x$setinv(m)
      m
}
