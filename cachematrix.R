## catch the inverse of a matrix

## creates a special "matrix" object (which is really a list containing a function)

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                                x <<- y
                                m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- solve
                getsolve <- function() m
                list(set = set, get = get,
                                setsolve = setsolve,
                                getsolve = getsolve)
}


## computes the inverse of the "matrix" object created above. If the inverse has already been calculated (and the matrix has not changed), it retrieves the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
                m <- x$getsolve()
                if(!is.null(m))
                                message("getting cached data")
                                return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
                m
}
