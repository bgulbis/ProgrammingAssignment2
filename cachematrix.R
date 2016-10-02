# cachematrix.R
#
# Functions included: makeCacheMatrix, cacheSolve
#
# Description: Takes a matrix and returns the inverse of that matrix. If
# the inverse has previously been calculated, then the cached inverse
# matrix is returned.

# Function: makeCacheMatrix
# Description: Returns a list containing the original matrix
# along with functions which can be used to cache the matrix.

makeCacheMatrix <- function(x = matrix()) {
    # create a new variable which will store the matrix
    inv <- NULL

    # resets any cached values so the matrix can be assigned a new value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # returns the matrix from the list created when makeCacheMatrix is called
    get <- function() {
        x
    }

    # takes a matrix and caches it
    setcache <- function(tmp) {
        inv <<- tmp
    }

    # finds the cached matrix and returns it
    # will return NULL if the matrix has not been cached
    getcache <- function() {
        inv
    }

    # returns a list containing a matrix, along with functions which
    # can be used to cache the matrix and retrieve the matrix from the cache
    list(set = set, get = get, setcache = setcache, getcache = getcache)
}

# Function: cacheSolve
# Description: Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    # check the cache and see if the inverse matrix exists
    # will return NULL if the inverse has not been cached
    inv <- x$getcache()

    # if the inverse matrix exists in cache,
    # let the user know the cached data is being used,
    # and return the inverted matrix; ends the function here
    if (!is.null(inv)) {
        message("returning cached matrix")
        return(inv)
    }

    # if the matrix has not been cached,
    # find the inverse and cache it by doing following:

    # get the original matrix and assign it to mat
    mat <- x$get()

    # invert the matrix using the solve() function
    # and assign the inverted matrix to inv
    inv <- solve(mat)

    # cache the inverted matrix using the setcache function
    # contained within the list of special matrix functions
    x$setcache(inv)

    # return the inverted matrix
    inv
}
