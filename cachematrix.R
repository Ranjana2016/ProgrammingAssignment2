
## makeCacheMatrix creates a matrix object and returns a list containing functions to 
## set, get,setinverse and getinverse functions. This list is used as an input to cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
	   inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve() takes the output of makeCacheMatrix() and returnthe 
## inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv = x$getinv()
       
##checks if the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)

return(inv)

}
