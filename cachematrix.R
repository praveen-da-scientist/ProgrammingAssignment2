## This Script contains Two Functions MakeCacheMatrix and cacheSolve.
## The script will help to create a cached matrix and its inverse.

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	matrixInverse <- NULL
	set <- function(y) {
		x <<- y
		matrixInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(Inverse) matrixInverse <<- Inverse
	getInverse <- function() matrixInverse 
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(m, ...) {
      ## Return a matrix that is the inverse of 'x'
	matrixInverse <- m$getInverse()
	if(!is.null(matrixInverse)) {
		message("getting cached matrix inverse")
		return(matrixInverse)
	}
	data <- m$get()
	matrixInverse <- solve(data, ...)
	m$setInverse(matrixInverse)
	matrixInverse 
}
