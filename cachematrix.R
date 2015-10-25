## This function pair makes possible the  inverse of a given matrix 
## once calculated to be stored. This is achieved by creating a list
## (which corresponds to a given matrix) of functions to set or access
##the values of created new kind of "matrix and its inverse" type data.


## The makeCacheMatrix sets the new data structure and returns a list of 
##functions giving access to it. The inverse value remains NULL untill its
##calculated and stored via setinv. 

makeCacheMatrix <- function(x = matrix()) {
 
    inv <- NULL
    
    set <- function(y) {
      x <<- y
    inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverz) inv <<- inverz
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
  
  

## cacheSolve function returns a matrix that is the inverse of 'x'.
##In case 'inverz' has not being cached yet calculates it and puts it 
##into structure above with setinv.  
##The inverse of the matrix is recommended to be set only by using 'cacheSolve'
##function because in case of using just'setinv' we may put any value 
#for inverse of a given matrix and this may distroy our structure . 


cacheSolve <- function(x, ...) {
        
  
    inv <- x$getinv()
    if(!is.null(inv)) {
     message("getting cached data")
     return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }
  

