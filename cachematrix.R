## Functions are used to implement the solve() function for a matrix
## makeCacheMatrix is used to create a CacheMatrix object with inbuilt cache for the result and getter/setters
## cacheSolve checks if cached result is available, and reports the value of solve() or getresult()

## makeCacheMatrix
## params: @x <invertible matrix>
## creates object storing the matrix, it's inverse(if set) and the relevant getter/setter functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
     x <<- y
     m <<- NULL
  }
  get <- function() x
  setresult <- function(result) m <<- result
  getresult <- function() m
  list(set = set, get = get,
       setresult = setresult,
       getresult = getresult)
}


## cacheSolve
##params: @x <cacheMatrix>
##Attempts to check if the matrix has the inverse cached
##If cache is not set, calculates inverse with solve() and sets cached result
##If cache is set, returns cached value

cacheSolve <- function(x) {
    m <- x$getresult()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setresult(m)
    m
}

