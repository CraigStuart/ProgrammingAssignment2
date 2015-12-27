## The makeCacheMatrix function creates a special "matrix" object
##that can cache the inverse of the matrix.

## The function sets the value of the matrix, then get the matrix value.
## Then the function sets the value of the inverse and then gets the inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}
        


## The function cacheSolve should compute the inverse function.
##If this value has already exists in cache then that value needs
##to be retreived and the computation is skipped.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
