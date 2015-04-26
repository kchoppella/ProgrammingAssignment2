## The pair of functions below implement a cache for the 
## inverse of a matrix.  If the contents of the matrix are unchanged, 
## these functions can save the time spent in re-computing the 
## matrix inverse and fetch the pre-computed inverse from cache


## This function implements a cache for the inverse of a matrix. it also provides
## utility functions to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set <- function(y) {
		x <<- y
		inv = NULL
	}
	get <- function() x
	setinv <- function(i) inv <<- i
	getinv <- function() inv
	list(set = set , get = get, 
		setinv = setinv, getinv = getinv)
}


## This function returns the matrix inverse of the matrix from cache.  If invoked
## for the first time or following a call to set(), the inverse is re-computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if(!is.null(inv)) {
			message("Getting cached data")
			return(inv)
		}
		data <- x$get()
		inv <- solve(x)
		x$setinv(inv)
		inv
}
