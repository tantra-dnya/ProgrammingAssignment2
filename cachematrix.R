## This R file creates a functions to create a square matrix
## and store its inverse and cache it in order to reduce the
## compute calculation. There will be 2 functions created to
## achieve our tasks. 
##
## First function will instantiate, have 
## getters-setters and return object list
##
## Second function will use object of first function as an 
## argument and will be used to cache the square matrix created
## in first function

## 1st function-makeCacheMatrix which will initialize the variable
## and create functions to set and access matrix and also solve
## inverse of matrix

makeCacheMatrix <- function(x = matrix()) 
{	
  # initalize to NULL
  invMat <- NULL
  
  # set function
  set <- function(vMat)
  {
    x <<- vMat
    invMat <<- NULL
  }
  
  #get function
  get <- function()
  {
    x
  }
  
  
  # setInvMat - set Inverse Matrix in parent environment
  setInvMat <- function(vInvMat)
  {
    # Assign argument variable vInvMat to function environment
    # variable invMat
    invMat <<- vInvMat
  }
  
  
  # getInvMat - get value of Inverse Matrix from parent environment
  getInvMat <- function()
  {
    invMat
  }
  
  # named return object list
  list(set=set, get=get,setInvMat =setInvMat, getInvMat=getInvMat)
  
  
}


## cacheSolve - 2nd function which will return inverse square matrix
## or return cached data

cacheSolve <- function(x, ...) 
{
  v2_InvMat <- x$getInvMat()
  
  if(!is.null(v2_InvMat))
  {
    message("Inv matrix set. Getting cached data")
    return(v2_InvMat)
  }
  
  data <- x$get()
  
  v2_InvMat <- solve(data,...)
  
  x$setInvMat(v2_InvMat)
  
  v2_InvMat
}
