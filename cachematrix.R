## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL                                       # begins by setting the matrix inverse to NULL as a placeholder for a future value
	set <- function(y) {                              # in case the user forget to set the matrix 'x' at first
		x <- y	                                # assigns the new matrix 'y' to 'x'
		inv <<- NULL                                # reset the inv to null in the parent environment
	}
	get <- function() x                               # function to print the matrix
	setinverse <- function(inverse) inv <<- inverse   # function to assign the inverse if it calculate already
	getinverse <- function() inv				  # function to print the inverse
	list( set = set,
	 	get = get,
		setinverse = setinverse,
		getinverse = getinverse)
	

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {                              # check if the inverse is calculate or not
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	if ( length(diag(data)) == nrow(data) & length(diag(data)) == ncol(data)) {  #check if the matrix is square and has an inverse and the compute it
		if (det(data) != 0) {
			inv <- solve(data)
		}
	}		
	x$setinverse(inv)
	inv
}
					
