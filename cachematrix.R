## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
	list(set = set,
		 get = get,
		 set_inverse = set_inverse,
		 get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
	inv <- x$get_inverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$set_inverse(inv)
	inv
}

## test results
##> source("assignment_Aug2020.R")
##> test_matrix <- makeCacheMatrix(matrix(c(4,5,7,-3,2,3,9,-2,8), 3, 3))
##> test_matrix$get()
##     [,1] [,2] [,3]
##[1,]    4   -3    9
##[2,]    5    2   -2
##[3,]    7    3    8
##> test_matrix$get_inverse()
##NULL
##> cacheSolve(test_matrix)
##             [,1]       [,2]        [,3]
##[1,]  0.084942085  0.1969112 -0.04633205
##[2,] -0.208494208 -0.1196911  0.20463320
##[3,]  0.003861004 -0.1274131  0.08880309
##> test_matrix$get_inverse()
##             [,1]       [,2]        [,3]
##[1,]  0.084942085  0.1969112 -0.04633205
##[2,] -0.208494208 -0.1196911  0.20463320
##[3,]  0.003861004 -0.1274131  0.08880309