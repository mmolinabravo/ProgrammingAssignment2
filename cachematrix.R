## makeCacheMatrix creates a an object that can cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        
		# m will store the cached value, we set it to NULL initially
		m <- NULL
		
		# set will store the matrix
		set <- function(y) {
                x <<- y
                m <<- NULL
        }

		# get returns the stored matrix
            get <- function() x
		# setinverse stores the inverse 
            setinverse <- function(solve) 
		m <<- solve
            # getinverse gets the cached value, the inverse
		getinverse <- function() m
            
		#this is a list of functions used
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve calculates the inverse of the matrix created with makeCacheMatrix
##If the inverse was already cached, it gets the value and don´t need to calculate

cacheSolve <- function(x, ...) {
		
		#tries to get the cached value and store it in m	
        	m <- x$getinverse()
		
		#if it finds a value, it returns it, together with a message 
       	if(!is.null(m)) {
                message("getting cached data")
                return(m)
       	}
        
		# if it doesn´t find a value, it proceeds to calculate it.
		data <- x$get()
       	#stores the calculated value in m
		m <- solve(data, ...)
       	x$setinverse(m)
        
	#finally returns the calculated m, with no message
	m
}
