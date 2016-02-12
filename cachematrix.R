## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<- function(y) {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) inverse<<-solve
        getinverse<-function() inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

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
