## This function makes a special matrix that can cache its inverse.
## I want this to test if this is a "square" matrix, and abort if it is not, but I haven't figured that out yet.
## Nice article on singular, or non-invertible matrices here: 
##     http://stackoverflow.com/questions/13145948/how-to-find-out-if-a-matrix-is-singular
## Also, check out this: http://en.wikipedia.org/wiki/Invertible_matrix
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}

## This function tests to see if the inverse has already been calculated, and fetches it. If it doesn't exist,
## it calculates the inverse of the matrix in makeCacheMatrix. 

cachesolve <- function(x, ...) { 
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## inv <- solve(data, ...) $*$ (data, ...)
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}