## R Programming Course Assignment 2
## Creating the makeCacheMatrix and cacheSolve functions.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## replaces the vector stored in makeCacheMatrix (x) with the new input (y) and reset inverse (inv) to NULL
        set <- function (y){
        x <<- y
        inv <<- NULL
        }
        ## returns vector x stored in makeCacheMatrix
        get <- function () x
        ## store value of inv in makeCacheMatrix
        setinverse <- function(inverse) inv <<- inverse
        ## returns stored value of inv
        getinverse <- function() inv
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## if there is a cached value for inv, return inv
        if(!is.null(inv)) {
        message ("getting cached data")
        return (inv)
        }
        ## else, compute the inverse of the input and store it in makeCacheMatrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
