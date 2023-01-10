## These two functions are use to accelerate the calculation of the inverse of matrices
## The first function calculate the inverse of the matrices and caches them in a list
## The second function verify if the inverse of the matrix is cached and gets it, otherwise
## it calculates the inverse of the matrix.

## The function makeCacheMatrix create a list containing functions:
## 
# set the value of the vector
# get the value of the vector
# set the value of the inverse of a matrix with solve
# get the value of the inverse of a matrix with solve


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) m <<- solve
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## The function cacheSolve check if the inverse of the matrix has already been calculated
# If so, it gets the value from the list, otherwise it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
  
}

