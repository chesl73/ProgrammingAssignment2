# this function creates the matrix, solves for the inverse and caches the result
makeCacheMatrix <- function(x = matrix()) {
  #create the matrix m
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  # invert/solve the matrix
  setmatrix<-function(solve) m<<- solve
  # return the matrix
  getmatrix<-function() m
  list(set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)
}

# this function checks for the cached matrix and uses this if available
cacheSolve <- function(x=matrix(), ...) {
  # try and return the cached matrix
  m<-x$getmatrix()
  # check if we have anything
  if(!is.null(m)){
    message("cached version of the data")
    return(m)
  }
  # u
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
  
}