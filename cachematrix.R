## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly.
## The following functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
	# Inverse matrix initialized
	inv <- NULL

	set <- function(y) {
		x <<- y
		inv <<- NULL
    }

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

	inv <- x$getinverse()

	if(!is.null(inv)) {
		message("getting cached data")
        return(inv)
	}
	
	data <- x$get()
    inv <- solve(data)  #Function solve returns the inverse of a matrix.
    x$setmean(inv)
    
	inv
}
