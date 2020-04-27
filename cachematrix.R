## Catching the inverse of a matrix 
## functions do

## We must write two functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      ## makeCacheMatrix creates an object called matrix that can cache its inverse
      inv <- NULL
      set <- function(y) {
              x <<- y
              inv <<- NULL
      }

      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, 
           get = get, 
           setinv = setinv, 
           getinv = getinv)
}


cacheSolve <- function(x, ...) {
        ## cacheSolve computes the inverse of this new object called matrix
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve (data, ...)
        x$setinv(inv)
        inv
}


## Program testing

my_matrix <- matrix(rnorm(25), 5, 5)
m <- makeCacheMatrix(my_matrix)
cacheSolve(m)
