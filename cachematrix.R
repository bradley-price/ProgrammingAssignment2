## The code that follows uses the lexical scoping rules of R that
## allow for using the body of another function (in this case, 
## the function that will be used as a cache) as the environment
## in which the operative function is defined.

## 1st function that creates the cache follows the set/get/set/
## get formula described in the assignment's example. Set the  
## matrix. Get the matrix. Set the inverse of the matrix. Get  
## the inverse of the matrix.  The desired output (in this 
## case, the inverse) is defined as NULL at the 
## beginning of function used to create the cache. The double  
## assignment operator '<<-' is used to define an object in a 
## different environment.

makeCacheMatrix <- function(x = matrix()) {
	inv < - NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
}
	get <- function() x
	setinv <- function(inverse)  inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)


## The second function below takes the output of Function 1    
## (makeCacheMatrix) and inverts that matrix when return is
## called. If the inverse of the matrix has already been computed
## and stored in the cache by a previous loop of the function,
## the second function does not repeat the matrix inversion
## computation, but skips it. If the inverse of the matrix was
## not calculated by a prior loop of the function, the remaining
## code calculates the inverse of the matrix using the solve
## function described in the assignment.

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        matrix.output = x$get()
        inv = solve(matrix.output, ...)
        x$setinv(inv)
        return(inv)
}
