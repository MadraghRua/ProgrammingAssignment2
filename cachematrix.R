## makeCacheMatrix creates a cache of bth a matrix and it's inverse
## it's set up so you can retrieve the values regardless of the current 
## environment from which the values came 
## cacheSolve does the work of converting the matrix to it's inverse
## it then uses makeCacheMatrix for storage and retrieval of these values

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## create a square matrix for caching data from cacheSolve
  ## create an empty variable for the inverse of the matrix
  ## largely based upon the example given in the assignment
  ## set is to set up the values of the matrix
  ## get is to retrieve the values of the matrix
  ## getinverse is to get the values of the inverted matrix
  ## setinverse is to set up the initial values of the inverted matrix
  
  ## create an inverse matrix variable and make sure it's empty by setting it to
  ## NULL
  inver <- NULL
  
  ## now set up the inverse caching operation
  ## create the set function to populate it with values
  set <- function(y) {
    ## assign the values of x to y
    ## because x is a matrix y will also be a matrix
    ## we set this up so we can always retrieve y and inverse regardless of the environment
    x <<- y
    inver <<- NULL
  }
  
  ## now set up all the arguments valls for the makeCacheMatrix function
  ## as per the example we want to set and get the values of the initial matrix
  ## we also want to get and set the values of the inverted matrix
  ## this looks very object oriented in how it's set up
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve is used to call the makeCacheMatrix function
## it gets the output of the makeCachematrix as it's value
## which shold be the inverse of the input matrix

cacheSolve <- function(x, ...) {
        ## it is called with value x and subsequently one of the arguments for 
        ## the makeCacheMatrix function 
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getInverse
  
  ## now check and see if the inverse is empty or not
  ## if it's populated then get the data
  if(!is.null(inverse)) {
    message("we have data - getting the data")
    return(inverse)
  } else {
    message("there is no inverse")
  }
  
  ## not calculate the inverse using solve
  result <- x$get()
  inverse = solve(result, ...)
  
  ## now populate the inverse into the makeCacheMatrix function with the 
  ## setInverse argument
  x$setinverse(inverse)
  
  return(inverse)
}
