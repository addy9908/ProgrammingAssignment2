## AUthor: Zengyou
## 

## function to create a cacheMatrix a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inv_x <<- inv
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## function to  computes the inverse of the special "matrix" returned
## by `makeCacheMatrix` above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        ## in case it already has the inverse matrix
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        # compute the inverse if not exist
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinverse(inv_x)
        inv_x
}
