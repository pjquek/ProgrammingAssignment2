## Assignment2: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse
## Assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
	m <-NULL		##reset
	set <-function(y){
		x <<-y
		m <<-NULL
	}
	get <-function()x
	setinv <-function(inv)m<<-inv
	getinv <- function()m
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve: computes the inverse of the special matrix returned by
##	makeCacheMatrix. If the inverse has already been calculated and matrix
##	not changed, it will just retrience the inverse from the cache

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinv(m)
	m
	
        ## Return a matrix that is the inverse of 'x'
}
