## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## memorizing and making matrix
makeCacheMatrix <- function(x = matrix()) {
  n<-NULL
  set<-function(z){
        x<<-z
        n<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)n<<-inverse
  getInverse<-function()n
  list(set=set,get,get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## do not repeat if is right
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    n<-x$getInverse()
    if(!is.null(n)){
      message("getting cached data")
      return(n)
    }
    q<-x$get()
    n<-solve(q,...)
    x$setInverse(n)
    n
}
