## Assignment: caching the inverse of a matrix

## This function creates a special matrix object that can cache its inverse
## This is achieved by:
## 1. Setting the value of the matrix.
## 2. Getting the value of the matrix.
## 3. Setting the value of the inverse.
## 4. Getting the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
	M <- NULL
	set <- function(y) {
		x <<- y
		M <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) M <<- inverse
	getinverse <- function() M
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        M <- x$getinverse()
        if(!is.null(M)){
        	message("getting cached data")
        	return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setinverse(M)
        M
}
