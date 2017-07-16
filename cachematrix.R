# Repeated and costly computation could benefit from caching function. The
# following paired functions are used to cache the inverse of a matrix.


# This fucntion creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL       
        set<-function(y){
                x<<-y
        }get<-function()x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function()inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# This fucntion below returns the inverse matrix to the caching function above. It first checks if
# the inverse has been computed. If so, it returns the result and skips the
# computation. If not, it computes the result and sends the value to the cache using
# setinverse function.

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        } else{
                newdata<-x$get()
                inv<-solve(newdata)
                x$setinverse(inv)
                ## Return a matrix that is the inverse of 'x'
                return(inv)
        }
} 

