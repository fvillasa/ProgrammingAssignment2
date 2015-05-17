## Function: makeCacheMatrix: 
##   - Arguments:
##     . A matrix whose inverse will be calculated
##   - Description: It contains the definition of four functions: set, get, setinverse, and getinverse
##   - Returns: A list with the four functions: : set, get, setinverse, and getinverse.
makeCacheMatrix <- function(x = matrix()) {
  #
  # Set to NULL the previous value calculated for the inverse of the matrix
  m <- NULL

  # 
  # The set function changes the matrix stored in the function. This function receives as argument a matrix, and set the matrix x with the contents of the matrix passed as argument
  set <- function(y = matrix()) {
     x <<- y
     m <<- NULL
  }
  # 
  # The get function returns the matrix stored in the main function
  get <- function() x

  # The setinverse function stores the value passed as argument into the variable m
  setinverse <- function(solve) m <<- solve

  # The getinverse function returns the value stored into the variable m
  getinverse <- function() m

  # The function returns a list with the four functions defined in the makeCacheMatrix 
  list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function: cacheSolve: 
##   - Arguments
##     . A list of the four functions returned by the makeCacheMatrix function
##     . Description: This function evaluates if the inverse of the matrix has been calculated before. If this is the case, it returns the value previously stored into the matrix m. Otherwise, 
##       it calculates the inverse of the matrix - using the solve function - and store the results into the variable m. 
##     . Returns: Returns the inverse of the matrix x.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  #
  # Get the value previously calculate as inverse of the matrix x
  m <- x$getinverse()

  #
  # If the value of m is not null, then the function returns the value previous calculated, sending a message that the value corresponds to a cached data
  if(!is.null(m)) {
     message("getting cached data")
     return(m)
  }

  #
  # If the inverse of the matrix is not stored in m - the value of m is null - then the contents of the matrix x is store in the matrix data
  data <- x$get()

  #
  # The matrix m is assigned with the inverse of the matrix data (matrix x)
  m <- solve(data, ...)

  #
  # The value m is set with the inverse of the matrix x calculated in the previous step
  x$setinverse(m)

  # The function returns the inverse of the matrix x
  m
}
