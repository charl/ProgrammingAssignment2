# Matrix inversion is usually a costly computation so caching the
# inverse of a matrix rather than compute it repeatedly is more
# efficient.
#
# The makeCacheMatrix() function creates a custom matrix object
# that encapsulates several functions to manipulae the internal
# matrix data as well as the inversion function.
#
# The cacheSolve() function acts as a cache that manipukates the
# supplied custom matrix object to either return the cached
# inverse of a square matrix or calculate it, store it and then
# return the stored inverse of the square matrix.

# makeCacheMatrix create a custom matrix object that can cache its
# own inverse square.
# @param  x        An invertable matrix.
# @return function
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    # Set the internal matrix to some other value and init the
    # cached inverted matrix.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    # Get the internal matrix.
    get <- function() x

    # Cache the inverted matrix.
    setinversion <- function(func) i <<- func

    # Get the cached inverted matrix.
    getinversion <- function() i

    # Return the function API.
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}


# cacheSolve acts as a cache helper by either collecting the
# cached inverse matrix from the supplied custom matrix object
# or calculating the inverse using solve() and storing that in
# the custom matrix object before returning it.
# @param  x      Custom matrix object created by makeCacheMatrix().
# @param  ...    Any extra params you want to pass into the solve() function.
# @return matrix The inverse of the original matrix.
cacheSolve <- function(x, ...) {
    i <- x$getinversion()

    # Cache hit.
    if(!is.null(i)) {
        return(i)
    }

    # Cache miss.
    data <- x$get()
    i <- solve(data, ...) %*% data
    x$setinversion(i)
    i
}
