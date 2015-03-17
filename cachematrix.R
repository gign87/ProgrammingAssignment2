## These functions calculate the inverse of an arbitrary set matrix and cache it in the memory.
## If the inverse has already been calculated, then it simply get the inverse from the cache, otherwise it computes it.  

## This function calculates the inverse of an input matrix. If it is a squared matrix, then the solve() function will be used,
## otherwise the inverse will be computed by the ginv() function. The latter one can be found in the MASS package. 

makeCacheMatrix <- function(x = matrix()) {
           inv<-NULL
           setmatrix<-function(y){
                 x<<-y
                 inv<<-NULL
           }
           getmatrix<-function() x
           calculateinverse<-function() {
                 if(ncol(x)==nrow(x)){
                       inv<<-solve(x)
                 } else {
                       inv<<-ginv(x)
                 }
           }
           getinverse<-function() inv
           list(setmatrix=setmatrix,getmatrix=getmatrix,
                calculateinverse=calculateinverse,getinverse=getinverse)
}


## It calculates the inverese of the matrix returned by the makeCacheMAtrix function.
## If the inverse has already been calculated, then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv<-x$getinverse()
      if(!is.null(inv)) {
            message("getting cached inverse matrix")
            return(inv)
      } else {
      inv<-x$calculateinverse()
      inv}
}
