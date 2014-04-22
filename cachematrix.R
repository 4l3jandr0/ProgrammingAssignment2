## This functions, together, are able to caching the inverse of a matrix, when
## it was already calculated

## Next function create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) (inv <<- inverse)
        getInverse <- function() { inv }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function computes de inverse of function above. If the inverse has
## already been calculated, then 'cacheSolve' retrieve the inverse from the 
## cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message ('getting cached data')
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
