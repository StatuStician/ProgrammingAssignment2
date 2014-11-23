## this function will take a matrix 
## and assign certain properties of the matrix to four different
## functions

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<- NULL
        }
        get<-function() x
        setInverse<-function(inverse) m<<-inverse
        getInverse<-function() m
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## THis function checks if the inverse of x exists and returns it
## If it doesn't exist the function calculates it

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setInverse(m)
        
}
