# This code will create an inverse of a matrix you've created
# And cache it for future use 
# It will check for a cache, if not, it will calculate

# Example usage:
# > x = rbind(c(1, -1/4), c(-1/4, 1))   ++ create a matrix
# > n = makeCacheMatrix(x)              ++ create cache matrix
# > n$get()                             ++ return matrix
# > cacheSolve(n)                       ++ creates inverse
# > cachesSolve(n)                      ++ will see "getting cached data" message, produces cached inverse

###

## makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 
  # i will store the cached inverse matrix
  i <- NULL
  
  # Set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Get matrix
  get <- function() x
  
  # Set inverse of matrix
  setinv <- function(inv) i <<- inv
  
  # Get inverse of matrix
  getinv <- function() i
  
  ## Return matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse of matrix
## Checks for the inverse from the cache, if yes: skip computation
## Otherwise, calculate.

cacheSolve <- function(x, ...) {
  # Check to see if inv has been calculated, return it
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # If not, calculate
  data <- x$get()
  i <- solve(data, ...)
  
  # Cache the inverse produced
  x$setinv(i)
  
  # Return the information
  i
}
