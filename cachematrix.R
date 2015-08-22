## Put comments here that give an overall description of what your
## functions do

## A function that is used to set/get value of a matrix, set/get inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <<- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)	

}


## CacheSolve calculates inverse of a  matrix, it checks if the inverse is already
## being calculated usig the above function. If yes, it gives the inverse from the 
## cache and skips the computatation using the method solve.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
    	message("getting Cached Data")
    	return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$getinverse(m)
    m
}
