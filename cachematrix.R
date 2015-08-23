## The following R code uses two functions which can create a special "matrix" object that can cache its inverse 
## and compute the inverse of the special "matrix"

## The <<- operator is used to assign a value to an object in an environment that is different from the current environment. 

## The first function makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
invmat<-NULL
set<-function(y) {
  x<<-y
  invmat<<-NULL
}
get<-function() x
setmatrix<-function(solve) invmat<<- solve
getmatrix<-function() invmat
list(set = set, get =get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}

## The function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x=matrix(),...){           ## Return a matrix that is the inverse of 'x'
  invmat<-x$getmatrix()
  if(!is.null(invmat)){
    message("getting cache data")
    return(invmat)
  }
  matrix<-x$get()
  invmat<-solve(matrix, ...)
  x$setmatrix(invmat)
  invmat
}
