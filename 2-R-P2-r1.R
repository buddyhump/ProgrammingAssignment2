## The functions together cache the calculated inverse of the input matrix 
## THEN re-calculate and overwrite the cached value IF the original matrix changes 


## makeCacheMatrix is a constructor function that creates a list of sets and gets that are used 
## by the cacheSolve function below for caching the value it calculates

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # creates four set and get functions to be used by calling function "cacheSolve"
  # superassigns x and m so they can be used outside local function environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  # combines four fuinctions in an array that is used by the "cacheSolve" function below 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of the input matrix and caches the value.
## R function "solve" is used to calculate the inverse 

cacheSolve <- function(x, ...) {
  # 
  m <- x$getinv()
  # If the matrix is unchanged get the cached value and notify user cache value is used
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If "m" is NULL calculate inverse value using R library "solve" function then cache value 
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  #print inverse matrix to console
  m
}



