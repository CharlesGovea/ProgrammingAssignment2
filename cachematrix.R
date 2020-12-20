## Creates a special type of object that stores a matrix and its inverse version

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(value){
        x <<- value
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invrs) inv <<- invrs
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of the inserted matrix
## If there's a cached value, it will return it instead of calculating it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    inv <- solve(x$get())
    x$setinv(inv)
    inv
}
