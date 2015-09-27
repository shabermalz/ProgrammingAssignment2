## The first function (makeCacheMatrix) stores a matrix if it is square and creates a list containing functions that enable the user
# to input a new matrix (setmatrix), retrieve the matrix (getmatrix), retrieve the inverse (getinverse) and 
## set the inverse. 

# the second function (cacheSolve) calculates the inverse of the matrix stored in the object created by the makeCacheMatrix
# function. It first gets the the value of the "inv" variable in which is either NULL or contains the inverse of the matrix. 
# If the value of "inv" is not NULL it returns the cached value of the inverse, if not it gets the matrix, calculates its 
#inverse  and stores it in "inv". Finally the inverse is returned.


makeCacheMatrix <- function(x = matrix()) {

  if (ncol(x) != nrow(x)) {
    
    stop("Please input a SQUARE matrix")
    
    
  }
  
  
  
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set = setmatrix, get = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# For the comments see above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  data
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
