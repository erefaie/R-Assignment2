## makeCacheMatrix create and return a list of functions used to return
## the inverse of invertable matrix

makeCacheMatrix <- function(x = matrix()) {
	## initialize the cache with NULL value
	cacheMatrix <- NULL

	## create the matrix
	set <- function(y) {
		x <<- y
		cacheMatrix <<- NULL
	}

	## return the matrix
	get <- function() x
	
	## create matrix inverse and store it in the cache
	setInverse <- function(solve) cacheMatrix <<- solve

	## return the matrix inverse
	getInverse <- function() cacheMatrix

	list (set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
		)
}


## cacheSolve calculate the inverse of matrix returend by makeCacheMatrix 
## and return the result if the inverse has not been calculated yet, otherwise 
## it retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  xInverse <- x$getInverse()


	  ## if the matrix inverse already in the cache then return it from the cache
        if(!is.null(xInverse)) {

		## check if the matrix did not changed
		oldMatrix <- x$get()
		if(dim(xInverse) == dim(oldMatrix) && all(x == oldMatrix)) {
                message("getting cached matrix's inverse")
                return(xInverse)
		}
        }
	  data <- x$get()
	  xInverse <- solve(data, ...)
        x$setInverse(xInverse)
	  return(xInverse) 


}
