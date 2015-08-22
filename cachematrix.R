## TOgether these functions retrieve the inverse of a matrix if it has already been set
## or, if not, it calculates the inverse of the matrix. Its purpose is to reduce computation
## time if it has already been cached.

## This function is a list of functions used to set and get the value of the 
## matrix and sets and gets the inverse of the matrix
## Run this function on a matrix

makeCacheMatrix <- function(x = matrix()) {
                  inv <- NULL
                  set <- function(y) {
                        x <<- y
                        inv <<- NULL
                  }
                  get <- function() x
                  setinverse <- function(inverse) inv <<- inverse
                  getinverse <- function() inv
                  list(set = set, get = get, 
                       setinverse = setinverse, getinverse = getinverse)
}


## This function retrieves the inverse of the matrix if it has already been calculated
## or else calculates the inverse and displays it
## Pass makeCacheMatrix function as the variable in this function

cacheSolve <- function(x, ...) {
              inv <- x$getinverse()
              if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
              }
              data <- x$get()
              inv <- solve(data,...)
              x$setinverse(inv)
              inv
}
