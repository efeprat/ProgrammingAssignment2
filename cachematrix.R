## This function creates a special "matrix" object that
## 1) represents the same matrix received as argument
## 2) avoids recalculating its inverse by caching it

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL              # inverse not cached yet
        set <- function(y) {
                x <<- y        # if matrix changes...
                i <<- NULL     #   cache is not up-to-date
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of a special "matrix"
## created by makeCacheMatrix above:
## 1) If the inverse has already been cached for that matrix,
##    it is returned without new inversion calculations.
## 2) Otherwise, the inverse is calculated, cached, and returned.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {      # if cache is up-to-date...
                message("getting cached data")
                return(i)      #   no need to recalculate
        }                      # if cache is not up-to-date...
        data <- x$get()
        i <- solve(data, ...)  #   calculate inverse...
        x$setinverse(i)        #   and cache it
        i
}
