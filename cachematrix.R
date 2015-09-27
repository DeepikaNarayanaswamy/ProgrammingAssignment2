## The functions here are makeCacheMatrix & cacheSolve.
# makeCacheMatrix : It is used to create a new matrix and save it and its inverse in the cache.
# cacheSolve : The fn. is used to compute the inverse of the matrix.



# The fn. makeCacheMatrix is used has the following fns. contained in it
# get : returns the matrix data
# set : sets the matrix value
#getInverse : returns the inverse matrix
#setInverse : sets the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {

	matrixInverse <- NULL
	set <- function(y){
		x <<- y
		matrixInverse <<- NULL
	}
	
	get <- function() x
	setInverse <- function(inverse) matrixInverse <<- inverse
	getInverse <- function() matrixInverse
	
	list(set = set , get = get , setInverse = setInverse , 	getInverse = getInverse)

}



#cacheSolve : This function takes the matrix 'x' that is formed
#by using makeCacheMatrix function
# It checks for the following 
#if the inverse has been computed already , then the inverse is taken from cache
#else it uses the solve fn. to calculate the matrix inverse and returns the inverse & sets it in
# cache using setInverse fn.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  #matrix inverse is computed using solve fn.
  inverse <-  solve(data,...)
  x$setInverse(inverse)
  inverse
  
  
}
