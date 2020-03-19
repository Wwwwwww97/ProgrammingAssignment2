## Put comments here that give an overall description of what your
## functions do
## Get the inverse of the matrix

## Write a short comment describing this function
## It sets and gets the value of the matrix, and then sets and gets the value of the inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<-solve
  getinverse<-function() m
  matrix(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Write a short comment describing this function
## The following function first checks to see if the inverse has already been calculated.
## If so,the cachesolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix.
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
