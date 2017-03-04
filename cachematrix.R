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
	x$setinverse(inv)
	
	inv
}


## Execution example:

	## > x = cbind(rnorm(1:4),rnorm(1:4), rnorm(1:4), rnorm(1:4))
	## > x
		##		[,1]       [,2]       [,3]        [,4]
		##[1,]  0.6048312 -0.9562472 -0.9751116  1.56366727
		##[2,] -0.2286143  0.6956440 -0.7985116  0.56711195
		##[3,] -0.6889890 -0.7002141  0.4368978 -0.69222071
		##[4,] -0.5335097 -2.3508212  0.4975050  0.02381657

	## > m = makeCacheMatrix(x)
	## > m$get()
		##			[,1]       [,2]       [,3]        [,4]
		##[1,]  0.6048312 -0.9562472 -0.9751116  1.56366727
		##[2,] -0.2286143  0.6956440 -0.7985116  0.56711195
		##[3,] -0.6889890 -0.7002141  0.4368978 -0.69222071
		##[4,] -0.5335097 -2.3508212  0.4975050  0.02381657
		
	## First run - No cache
	## > cacheSolve(m)
		##           [,1]       [,2]      [,3]       [,4]
		##[1,]  1.4144608 -1.8299126  1.640722 -1.6055661
		##[2,] -0.9373734  0.6917249 -1.529540  0.6161933
		##[3,] -2.8056929  1.2157786 -5.236466  3.0607742
		##[4,] -2.2304838  1.8890004 -4.835503  2.9065813
		
	## Second run - Cache data retrieved
	## > cacheSolve(m)
		## getting cached data
		##           [,1]       [,2]      [,3]       [,4]
		##[1,]  1.4144608 -1.8299126  1.640722 -1.6055661
		##[2,] -0.9373734  0.6917249 -1.529540  0.6161933
		##[3,] -2.8056929  1.2157786 -5.236466  3.0607742
		##[4,] -2.2304838  1.8890004 -4.835503  2.9065813