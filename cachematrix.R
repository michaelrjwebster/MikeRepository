
## Takes an invertable matrix, and returns a list containing functions to
##  set the matrix (set)
##  get the matrix (get)
##  set the inverse (setmatrix)
##  get the inverse (getmatrix).
##  This list is used as the input to cacheSolve()


makeCacheMatrix <- function(x = matrix()) {
  
  
  m<-NULL
  set<-function(y){
    # <<- assigns a value to an object in an environment 
    # different from the current environment.
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Takes an invertable matrix, and returns a matrix that is an inverse of 'x'. 
## Returns the inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x=matrix(), ...) {
  
  m<-x$getmatrix()
  if(!is.null(m)){   # if the inverse has already been calculated
    message("getting cached data") 
    return(m) # get it from the cache and skips the computation. 
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
}

B <- matrix(c(1,3,2,4),nrow=2, ncol=2)
q<-makeCacheMatrix(B)
cacheSolve(q)


