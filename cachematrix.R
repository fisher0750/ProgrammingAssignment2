## makeCacheMatrix is a function that takes a matrix x as its input
## The default input is a 1 x 1 matrix with NA as its element
## Its output is a list of four functions
## These four functions will be used by cacheSolve(x,...) described below.
## They are:
## 1. set(y) which writes y onto x and makes m null
## 2. get() which has no argument and just gets x, wherever it is found
## 3. setmat(mat) which writes mat onto m
## 4. getmat() has no argument and just returns m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
## This function takes y as an input, assigns it to x
## and also assigns the null value to the m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
## This function doesn't need an argument, and it outputs x
  get <- function() x

## This function assigns mat to m in the parent environment  
  setmat <- function(mat) m <<- mat
  
## This function doesn't need an argument, and
## it outputs m in the local environment
  getmat <- function() m

## Now create a list with four objects, each of which is a function
  list(set = set, get = get,
           setmat = setmat,
           getmat = getmat)
}


## cacheSolve(x) takes a list as an input.
## It will calculate the inverse of a new matrix
## Or it will return a cached matrix to save computing time

library(MASS)
## I want to allow for the possibliity that the matrix was singular
## MASS allows me to use the generalized inverse function

cacheSolve <- function(x, ...) {
  ## x is a list of four different functions descirbed above
  m <- x$getmat()
  
  ## check if the getmat() part of x is not empty
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    ## Return m, tell me you found a cached matrix, 
    ## and then get out of cacheSolve(.)
  }
  ## otherwise dat is the get part of the list x
  dat <- x$get()
  ## m is the Moore Penrose pseudo-inverse of dat
  ## My code can handle singular matrices
  
  m <- ginv(dat, ...)
  x$setmat(m)
  m
}
## See you later, alligator!