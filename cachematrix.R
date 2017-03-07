# Calculates the inverse of the provided matrix or retrieves the inverse from cached memory if it already exists

# makeCacheMatrix creates a list variable containing functions to set and get a matrix variable
# and set and get the inverse of the matrix variable
makeCacheMatrix <- function(x = matrix()) {
      
      i <- NULL
      
      #Set value of matrix var.
      set <- function(y) {
            x <<-y
            i <<-NULL
      }
      
      #Get value of matrix var.
      get <- function() x
      
      #Set value of inverse matrix var.
      setinverse <- function(inverse) i<<- inverse
      
      #Get value of inverse matrix var.
      getinverse <- function() i
      
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#cacheSolve calculates the inverse of a matrix var. using solve() or retrieves the value of the inverse if it already exists
#in cached memory
cacheSolve <- function(x, ...) {
      #Set i equal to inverse matrix
      i <- x$getinverse()
      
      #If inverse is already cached return value
      if (!is.null(i)) {
            message("Retrieving cached inverse matrix")
            return(i)
      }
      
      #Retrieve provided matrix to calculate inverse
      matdat <- x$get()
      
      #Calculate inverse
      i <- solve(matdat, ...)
      
      #Set value of now cached inverse
      x$setinverse(i)
      
      i
}
