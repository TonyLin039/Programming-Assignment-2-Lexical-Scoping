# Define the S4 class
setClass("CacheMatrix",
         representation(data = "matrix", 
                        inverse = "matrix",
                        set = "function",
                        get = "function",
                        setInverse = "function",
                        getInverse = "function"))

# Constructor function
makeCacheMatrix <- function(x = matrix()) {
  slotData <- x
  slotInverse <- matrix(, nrow = nrow(x), ncol = ncol(x))  # Initialize slotInverse to empty matrix
  
  # Setter function to set the matrix
  set <- function(y) {
    slotData <<- y
    slotInverse <<- matrix(, nrow = nrow(y), ncol = ncol(y))  # Reset the cached inverse when matrix is updated
  }
  
  # Getter function to retrieve the matrix
  get <- function() slotData
  
  # Function to compute the inverse of the matrix
  setInverse <- function(inverseMatrix) {
    slotInverse <<- inverseMatrix
  }
  
  # Getter function to retrieve the cached inverse
  getInverse <- function() slotInverse
  
  # Create and return an object of class CacheMatrix
  new("CacheMatrix", data = slotData, inverse = slotInverse,set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" object
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if available
  inverse <- x@inverse
  if(is.null(dim(inverse)) || any(dim(inverse) == 0)) {
    message("Getting cached inverse")
    return(inverse)
  }
  # If inverse is not cached, compute it
  data <- x@data
  inverse <- solve(data, ...)
  x@inverse <- inverse  # Cache the inverse
  inverse
}

mat <- matrix(c(4, 7, 2, 6), 2, 2)
cacheMat <- makeCacheMatrix(mat)
identical(cacheMat@get(), mat)
inverse <- cacheSolve(cacheMat)
expected_inverse <- solve(mat)
identical(inverse, expected_inverse)
