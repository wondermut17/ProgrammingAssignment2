## Put comments here that give an overall description of what your
## functions do

## This function works in two parts. The first part uses lexical scoping to create
## a special object that stores a matrix and cache's its inverse in an environment 
## other than the current environment.
## The second function calculates the inverse of the matrix created by the first function.
## But first it checks to see if the inverse of the matrix has already been calculated.
## If it has the inverse of the matrix from the cache is printed and skips over the 
## computation. 

## The first function, makeCacheMatrix, creates a special "matrix", which is really a list
## containing a function to
## 1.set the dimensions and values of the matrix
## 2.get the dimensions and values of the matrix
## 3.set the dimensions and values of the inverse of the matrix
## 4.get the dimensions and values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  
                                             
  
  m<-NULL                                    
  set<-function(y){                          
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


## The second function calculates the inverse of the matrix of the data and sets
## the inverse of he matrix in the cache through the setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}