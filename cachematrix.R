## Put comments here that give an overall description of what your
## functions do

## makeCacherMatrix
## inverse
## for initial matrix, inverse to NULL
## already inversed, doing nothing
## memorizing and making matrix
makeCacheMatrix <- function(x = matrix()) {
  n<-NULL
  
  set<-function(z){
        x<<-z
        n<<-NULL
  }
  
  get<-function()x
  setInverse<-function(invs)n<<-invs
  getInverse<-function()n
  list(set=set,get,get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve
## already in, this function find from the cache
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
