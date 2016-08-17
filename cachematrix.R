## makeCacheMatrix takes a matrix as input, assigns space is a different environment
## to save the matrix and returns a list of addresses to functions for the cacheSolve function

## The cacheSolve function checks to see if the inverted natrix exists and returns or calculates
## it and sets a flag indicating it's been determined already
## functions do

##  input variable x: a square invertible matrix limited to slightly less than size of memory
makeCacheMatrix <- function(x = matrix()) {
	## returns:  list containing the functions that perform:
	##           seting and geting the matrix
	##           seting and geting the inverse of the matrix passed in
	##           this list is used as the input to cacheSolve()
	## as per the course instructions for this programming assignment in the README
	
	inv.mat <- NULL
	set <- function(y) {
		# uses `<<-` to assign values to object in an environment but not the current env
		x <<- y
		inv.mat <<- NULL
	}
	get <- function() {x}
	setinv <- function(inverse) {inv.mat <<- inverse}
	getinv <- function() {inv.mat}
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve determines the inverse of the “matrix” returned by makeCacheMatrix().
## if inverse has already been determined, the function returns inverse directly
## from the cache saving compute time.
cacheSolve <- function(x, ...) {
	## Return matrix that is inverse of 'x'
	## created by makeCacheMatrix()
	## returns: inverse of the original matrix input to makeCacheMatrix()
	inv.mat <- x$getinv()
	# if the inverse has already been calculated
	if (!is.null(inv.mat)){
		# get it from the cache; skip computation. 
		message("getting cached data")
		return(inv.mat)
	}
	# otherwise, calculate inverse using solve()
	mat.data <- x$get()
	inv.mat <- solve(mat.data, ...)
	# sets value of inverse in cache via setinv function.
	x$setinv(inv.mat)
	
	return(inv.mat)
}
