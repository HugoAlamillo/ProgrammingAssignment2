## These functions save a matrix to cache and can return cached matrices

## makeCacheMatrix() stores a given matrix in cache, or returns an input matrix, or returns an inversed matrix, or returns an inversed matrix.

makeCacheMatrix<-function(x=matrix()){
	i<-NULL
	set <-function(y){
		x <<-y
		i<<-NULL
	}
	get.matrix <- function() x
	set.inverse <-function(inverse=matrix()) i <<-inverse
	get.inverse <-function() i
	list(set=set, get.matrix = get.matrix, set.inverse = set.inverse, get.inverse = get.inverse)	
}


## This function takes a matrix from cache using makeCacheMatrix() and calculates the inverse of that matrix. It then returns the inversed matrix.

cacheSolve<-function(x, ...){
	## Return a matrix that is the inverse of 'x'
	i<-x$get.inverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get.matrix()
	i<-solve(data)
	x$set.inverse(i)
	i
	
}
