## This function creates a list with 4 elements named set,get,setinverse,and getinverse which are the 4 functions respectively that will create 
## and cache(ie store) the inverse of a given matrix outside of the current local environment of the function for a given matrix so that it can 
## be called again if needed, thus not having to recalculate it. This improves the operational efficiency of the process/function.
## Function assumes matrix is invertible, thus it's inverse exists and is unique.

## This function creates the list of the 4 functions. The set function contols the process by initializing the objects and triggers new 
## calculations when the matrix changes. get will return the matrix, while the setinverse and getinverse set/return the inverse for given matrix.

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<- function(y) {
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setinverse<-function(inv) inverse<<-inv
        getinverse<-function() inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve is a function that will return the cached inverse for a given matrix if that inverse already exists(ie is not NULL) or calculates
## it using the solve function and then sets and caches it using the setinverse function so that it can be called later if needed.


cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        data<-x$get()
        inverse<-solve(data, ...)
        x$setinverse(inverse)
        inverse
}

## Example of function creating cache inverse and confirming matrix %*% inverse equals identity matrix
##
## > setwd("~/ProgrammingAssignment2")
## > source('~/ProgrammingAssignment2/cachematrix.R')
## > v<-makeCacheMatrix(matrix(1:4,2,2))
## > v$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > v$getinverse()
## NULL
## > cacheSolve(v)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > v$getinverse()
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(v)
## getting cached inverse
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > v$get() %*% cacheSolve(v) ## original matrix times cached inverse does equal identity matrix
## getting cached inverse
##       [,1] [,2]
## [1,]    1    0
## [2,]    0    1

