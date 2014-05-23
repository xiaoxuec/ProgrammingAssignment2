##Matrix inversion may be a time-consuming computaiton and this 
##programming  is able to cache the inverse of a matrix.
##For example, it may take too long to compute the inverse of a big matrix,
##especially when repeated computation happpens (in a loop). 
##This programming saves the computed inverse of a matrix in the cache. Therefore,it first look
## up in the cache to avoid computing if the content of the matrix 
## is not changed.

## The first function creates a spelcial matrix, which is really a list which 
## contains functions to set the value of the matrix, get the value of the 
## matrix, set the value of its inverse, get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve)  m<<-solve
        getsolve<-function()   m
        list(set=set,get=get,
             setsolve=setsolve,
             getsolve=getsolve )  ## Return a list with four functions 

}

## This function computes the inverse of the special "matrix" created above. 
## It first looks up to see if its inverse is calculated,if so,then it returns
## its inverse and skip the computation. If not, then it calculates its inverse
## and set it in the cache via setsolve function.

cacheSolve <- function(x, ...) {
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setsolve(m)
        m ## Return a matrix that is the inverse of 'x'
}
