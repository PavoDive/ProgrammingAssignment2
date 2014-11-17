## Code for assignment 2 of R programming.
# This function calculates the inverse of [potentially] big matrixes, and chaches it, so it can compare whether matrix has changed
# and re-calculate inverse, or return cached value if not

makeCacheMatrix <- function(x = matrix()) {
  I<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) I<<-inverse
  getInverse<-function()I
  list(set=set,get=get,getInverse=getInverse,setInverse=setInverse)
}



## Determines whether it already has the value in cache, or solves (produces the inverse) of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I<-x$getInverse()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data<-x$get()
  I<-solve(data)
  x$setInverse(I)
  I
} 