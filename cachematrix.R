## This function, makeCacheMatrix, creates a special "matrix" object that can cache 
##  its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv

    list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function, cacheSolve, computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data) #solve() will return the inverse of matrix data
    x$setInverse(inv)
    inv
}



