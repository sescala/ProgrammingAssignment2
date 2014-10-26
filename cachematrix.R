## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## creates variable in local environment
  set <- function(y){
    x <<- y ## superasssigns value of y to x; x is makeCacheMatrix()'s input
    m <<- NULL ## if set() called, new vector will be stored in x and replace existing value
  }
  get <- function()x ## used to receive the values of x from makeCacheMatrix
  setmatrix <- function(solve) m <<- solve ##sets m to inverse in parent environment
  getmatrix <- function()m ## R will look for the value of me in getmatrix and then to the parent environment
  list(set = set, get = get, ## makes functions public
       setmatrix = setmatrix, ## allows function to be called
       getmatrix = getmatrix) ## outside local environment
}

cacheSolve <- function(x=matrix, ...) { ## returns a matrix that is the inverse of x
  m <- x$getmatrix() ## calls the getmatrix() functionf rom x
  if(!is.null(m)){ ## checks if m has an existing value. if yes, then returns value
    message("getting cached data")
    return(m) ## if no existin value, will calculate it below
  }
  matrix <- x$get() ## calls get() function from x
  m <- solve(matrix, ...) ## computes inverse from retrieved values 
  x$setmatrix(m)
  m ## prints
}
