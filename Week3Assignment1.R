makeCacheMatrix <- function(x = matrix()) 
 ## This function creates a matrix object that can cache its inverse
  { 
inv <- NULL                          ## Initialize a variable  
set <- function(y) {                 ## Define the function set   
x <<- y                              ## Value of matrix in parent environment
inv <<- NULL                         ## Reset the variable inv
}
get <- function() x                  ## Define the function get.  This returns the value of the matrix argument
setinverse <- function(inverse) inv <<- inverse ## Assign value of inv in parent environment
getinverse <- function() inv                    ## Gets the value of inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## This is necessary in order to refer 
                                                                              ##to the functions with $
}


cacheSolve <- function(x, ...) {
 ## Returns the matrix that is the inverse of x
  inv <- x$getinverse()             ## Assign x to inv
    if(!is.null(inv)) {             ## Loop - when inv is null then generate a message
            message("getting cached data")
            return(inv)
        }
  data <- x$get()                   ## Assign get function to data
      inv <- solve(data, ...)       ## Assign solve to inv
      x$setinverse(inv)             
      inv
}          
