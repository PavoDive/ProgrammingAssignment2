## Code for assignment 2 of R programming.
# This function calculates the inverse of [potentially] big matrixes, and chaches it, so it can compare whether matrix has changed
# and re-calculate inverse, or return cached value if not

makeCacheMatrix <- function(x = matrix()) {
  # Function returns a list of the possible functions that can be called, and what those functions are. Some of the functions pass certain values
  # to chache
  
  I<-NULL
  
  # This is the function that caches values of its argument and a NULL value for 
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  # A "returning" function
  get<-function()x
  
  # This function passes its argument to global environment 
  setInverse<-function(inverse) I<<-inverse
  
  # This function retrieves the value of I, according to scoping rules
  getInverse<-function()I
  
  list(set=set,get=get,getInverse=getInverse,setInverse=setInverse)
}



## Determines whether it already has the value in cache, or solves (produces the inverse) of the matrix

cacheSolve <- function(x, ...) {
  ## First, it attempts to retrieve the value of I [eventually] stored in global envirnonment 
  I<-x$getInverse()
  
  # In case it exists, returns the cached value and informs the user that the value returned was indeed from the cache 
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  
  # In case it was not (the return line would have ended the function), it then assigns the value of matrix (actual argument) to data
  data<-x$get()
  
  # and produces the inverse
  I<-solve(data)
  
  # It then passes the [recently calculated] inverse to SetInverse which will, in turn, cache the value of I
  x$setInverse(I)
  
  # The last line returns I
  I
} 