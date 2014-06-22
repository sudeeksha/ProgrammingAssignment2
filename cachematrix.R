##This script contains a couple of functions that can be used to compute and cache the inverse of a matrix.
##Assumption: It is assumed that the matrix supplied is always invertible

## 'makeCacheMatrix' function is a function that returns a list of functions 
## that get and set values of the matrix passed as an argument and
## also contain functions that allow us to cache the inverse of a matrix and access it.

makeCacheMatrix <- function(x = matrix()) { 
	 
	  inv <- NULL	
				
        set <- function(p) {
                x <<- p				
		    inv <<- NULL			##prior to computing the inverse, the default value is NULL
        }

        get <- function() x	
	
        setinv <- function(y) inv <<- y 	##caching the inverse

        getinv <- function() inv		##accessing the cache

        list(set = set, get = get,		##returning the list of functions
             setinv = setinv,
             getinv = getinv)
}

## 'cacheSolve' function takes the output list returned by the 'makeCacheMatrix' as an argument.
## When arg<-makeCacheMatrix(desired_matrix) is passed to 'cacheSolve' function as cacheSolve(arg),
## it checks whether a cache of the inverse of the desired_matrix exists and uses it, if it does. 
## If the inverse doesn't exist, it proceeds to solve for the inverse and cache it.

cacheSolve <- function(y) {
       
	  inv <- y$getinv()				##reading the cache
		
        if(!is.null(inv)) {				##checking if the cache is empty
                message("getting cached data...")	
                return(inv)				##returning the inverse if it is found in the cache and exiting the function
        }
	  
	  message("no cached data, computing...")	
	
	  mat<-y$get()					##accessing the matrix

        inv <- solve(mat)				##computing the inverse of the matrix

        y$setinv(inv)					##caching the inverse 

	  inv							##returning the inverse after computing and caching
}
