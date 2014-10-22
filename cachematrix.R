## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix takes an input (a matrix) and writes a list of 
# derived functions on the input matrix to the cache. This includes:
#   1. get(): A function to return a direct copy of the matrix. 
#      This stored copy allows for comparison with the input matrix 
#      thereby ensuring that any cached inverse matrix correctly 
#      relates to the input matrix offered by the user.
#   2. setinverse(): A function to reset the cached inverse 
#      of the input matrix. This is called when required by cacheSolve
#   3. getinverse(): A function to recover the cached value of the 
#      inverse of the input matrix, &
#   4. set(): A function to reset the cached matrix copy to new values 
#      and (most importantly) reset the cached inverse value for this 
#      (new) input matrix back to NULL 
# Note: makeCacheMatrix must be written to (only) the object 'a' for 
# cacheSolve below to locate the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve takes an input matrix and examines the stored cache 
# object ('a') and if:
# 1. If object 'a' exists (i.e. the cache is in use) the stored 
# copy of the parent matrix and the stored value of the matix inverse
# is retreived and inspected. If:
#     1.a if the stored matrix and the input matrix are identical 
#     the program inspects the cached inverse. If it exists it's value 
#     is returned but if not the inverse is calculated
#     (and returned) and the cached value is also updated
#     1.b. If the stored matrix differs from the input matrix, the new
#     inverse is calculated and returned but the cached value is not updated.
# 2. If the object 'a' does not exist the inverse of the input matrix
# is calculated directly. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(exists("a")){
  	  d <- a$get()
	  i <- a$getinverse()
	  if(identical(d,x)){
	    if(!is.null(i)) {
	      message("getting cached inverse")
	      return(i)
	    }else{
	      message("calculating inverse - storing result in cache")
	      i <- solve(d)
	      a$setinverse(i)
	      return(i)
	    }
	  }else{
	    message("matrix not in cache - calculating inverse")
	    i <- solve(d)
	    return(i)
	  }
  }else{
  	message("cache not in use - calculating inverse")
  	i <- solve(x)
  	return(i)
}
