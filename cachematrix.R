## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is computationally costly. The functions below calculate the inversed
# matrix and store it in a special object (a list of functions). When the function is
# called again in future, it will first check if the inversed matrix has already been
# calculated. If so, it will simply return the inversed martix, saving some time.

## Write a short comment describing this function
# The first function will take a matrix (if not if will try to convert it into a matrix)
# and return a list of functions, in which get() will simply return the matrix and 
# getinverse will return the cached inversed matrix. When no inversion has been performed
# getinverse will return NULL as it is defined. setinverse() will take the inversed matrix
# calculated by the cacheSolve function (note that should be in the parent environment)
# and store it in i. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function will take the object (list of functions) returned by makeCacheMatrix()
# First, it call getinverse(). If there is an inversed matrix stored in the object,
# i.e. i is not NULL, then it will return i as the result. If no inversed matrix is
# stored in the object, it will call get() and get the matrix stored in the object.
# Then a inversed matrix is calculated and stored back into the object by calling
# setinverse() and then return the inversed matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("cached inversed matrix found, getting the matrix...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


# Testing
# special_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(special_matrix)
