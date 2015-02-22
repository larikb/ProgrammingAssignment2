#The following two functions are going to cache the inverse of a matrix.

#The following makeCacheMatrix creates a "vector", which is a list containing a function to
#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the solve (a function for computing the inverse of a square matrix)
#4. get the value of the solve (a function for computing the inverse of a square matrix)

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


#The following function calculates the inverse of a square matrix:

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)           
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
