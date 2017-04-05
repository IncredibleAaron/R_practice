##This function is trying to create a solution "list" to create a cache to a matrix
makeCacheMatrix <- function(x = matrix()) {
  # i is the cache part for storing the calculated inverse matrix
  i <- NULL
  
  # initialize. cache is reset. 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # retrival function
  get <- function() x
    
  # store calculated inverse to cache
  setInv <- function(Inv) i <<- Inv
    
  # retrive inverse from cache
  getInv <- function() i
    
  #return a list of function  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

#this function is to calculate the inverse of matrix. If it is not in cache, we will calculate and store it into cache. If it already in cache, we directly read it
cacheSolve <- function(x, ...) {
  
  # i is to retrive from cache
  i <- x$getInv()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # retrive matrix
  data <- x$get()
  
  #calculate the inverse matrix
  i <- solve(data, ...)
  
  #store calculation into cache
  x$setInv(i)
  
  #return
  i
}
