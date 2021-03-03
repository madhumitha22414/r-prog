##These two functions makeCacheMatrix() and cacheSolve() allow you to
##efficiently do repeated inversions of the same matrix by caching the result
##instead of calculating it each time
##First function creates a special "Matrix" which is really a list

makeCacheMatrix <- function(m=matrix()) { 
inv <- NULL

set <- function(y) { #set the matrix value
m <<- y 
inv <<- NULL
}

get <- function() m    #get the matrix value
setinv <- function(solve) inv <<- solve   #set the invereted matrix value 
getinv <- function() inv   #Get the inverted matrix value

##returns the list:
list(set = set, get = get,
setinv = setinv,
getinv = getinv)
}

cacheSolve <- function(x, ...) { 
##this function returns the inverese matrix of x from cache, or calculate it in case cached data isn't available.

inv <- x$getinv() #Getting the inverse from cache 
if(!is.null(inv)) { #Checking if there was cached data
message("getting cached data")
return(inv) 
}

##if cached data was not available:
matrix.to.invert <- x$get()      #get the matrix 
inv <- solve(matrix.to.invert, ...) #solve it
x$setinv(inv)        #set it in cache inv #return the inverse
}
