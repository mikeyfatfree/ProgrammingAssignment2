## Assignment: Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then 
## solve(X) returns its inverse. For this assignment, assume that the matrix supplied is always invertible.


## Purpose: the function creates a special matrix which caches a matrix and its inverse with routines to get and set both the matrix and 
## the inverse.  this function does not compute the inverse.  
## Return: All routines are returned as a list

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Purpose: this function takes a makeCacheMatrix type matrix and computes/sets the inverse using base::solve()
## if the inverse of a matrix has already been computed, it will return the cached inverse
## Return: inverse of a makeCacheMatrix-type matrix

cacheSolve <- function(x, ...) {
	## check if makeCacheMatrix object has its inverse computed. if so, return it
	inv <- x$getinverse()  
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	## get the matrix from the object and compute the inverse
	data <- x$get()
	inv <- solve(data, ...)
	
	## store the inverse in the object and return it
	x$setinverse(inv)
	inv		
}
