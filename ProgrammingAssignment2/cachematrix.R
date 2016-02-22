## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
##
##
## This function creates a special "matrix" object that can cache its inverse.
 

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {            ## Reset Matrix 
      x <<- y                     ## Assign new value to x 
      mtrx <<- NULL               ## Initialize matrix to NULL
  }
  
  get <- function() x
  setInvMtrx <- function(InvMtrx) mtrx <<- InvMtrx
  getInvMtrx <- function() mtrx
  list(set = set, 
       get = get,
       setInvMtrx = setInvMtrx,
       getInvMtrx = getInvMtrx)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
  mtrx <- x$getInvMtrx()              
  if(!is.null(mtrx)) {        ## checking if same matrix was used before
    message("getting cached data")
    return(mtrx)               
  }
  data <- x$get()             ## if not get new matrix
  mtrx <- solve(data, ...)    ## calculate the inverse matrix
  x$setInvMtrx(mtrx)          
  mtrx                        ## show the inverse matrix
                              
}
