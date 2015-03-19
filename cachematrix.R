
makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(solve) m<<- solve
  getinverse<- function() m
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
  }
#Generating "cacheSolve": output is the inverse matrix of x:
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting chached data")
    return(m)
  }
  data<-x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}

##Testing functions: 
#Testing "makeCacheMatrix"
m <- matrix(c(-1, -2, 1, 1), 2,2)
x <- makeCacheMatrix(m)
x$get()
#Testing "cacheSolve":
inv <- cacheSolve(x)
inv
