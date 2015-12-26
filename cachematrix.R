## Put comments here that give an overall description of what your
## functions do

## creates a special matrix which caches a matrix and its inverse with routines to get and set both the matrix and the inverse
## this function does not compute the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
		message("set was called!")
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function takes a makeCacheMatrix type matrix and computes/sets the inverse
## if the inverse of a matrix has already been computed, it will return the cached inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv		
}
