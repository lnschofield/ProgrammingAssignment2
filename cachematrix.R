# Many thanks to Bill Hilton for the post "Making sense of Assignment 2" - I couldn't
# have done this without his explanation of how this type of object oriented code works


#so, this first part is supposed to make a matrix object that is able to store the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL 
  # 'inv' is the inverse matrix and it's set to NULL each time makeCacheMatrix is called
  # the next functions are used by cacheSolve() to get values for the original matrix 'x' 
  # or for 'inv' and then setting the solved matrix
  
  get <- function() { x } 
  # just returns the original matrix
  
  setinv <- function(solve) { inv <<- solve }
  # this is called by cacheSolve() the first time it is used and then it stores the value
  
  getinv <- function() { inv } 
  # retrives the cached value when it is used later
  
  list(get = get,
       setinv = setinv, 
       getinv = getinv) 
  # this is the list of functions used each time makeCacheMatrix() is called so the program knows
  # where to access those functions
}

# This part is able to actually find the inverse of the matrix object stored above, but it only does 
# so when the inverse is not already cached in that object and that object has not changed
cacheSolve <- function(x, ...) {
# x is what was made by makeCacheMatrix
  
  inv <- x$getinv() 
  if(!is.null(inv)) {
  # gets the object x and then solves the matrix but only if it was already not already cached
    
    message("getting cached data")
    
    return(inv)
    #returns the solved matrix and ends the function
  }
  data <- x$get()
  # if getinv() is null, we move down to here
  
  inv <- solve(data, ...) 
  # then it calculates the inverse matrix
  
  x$setinv(inv)
  # the solved matrix is stored
  
  inv
  # returns the inverse matrix
}