## Caching the inverse of a matrix 

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	mx <- NULL # Init

	set <- function(y){
		x <<- y
		mx <<- NULL
	}
	get <- function() x 
	setInverse <- function(solve) mx <<- solve
	getInverse <- function() mx
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Computes the inverse of the special "matrix" return by makeCacheMatrix function, 
## then retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mx <- x$getInverse()
	if(!is.null(mx)){
		message("getting cached data")
		return(mx)
	}
	data <- x$get()
	mx<-solve(data,...)
	x$setInverse(mx)
	mx
}
