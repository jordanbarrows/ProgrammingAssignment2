# Matrix inversion can be a costly process, so sometimes it is beneficial
# to be able to cache the inverse of a matrix and be able to call it up
# at a later time. These functions allow us to do exactly that.


# This function will generate a list containing the following:
#     set: set the value of the matrix
#     get: get the value of the matrix
#     setinverse: set the value of the inverse of the matrix
#     getinverse: get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # Sets the inversion variable, 'i', to NULL
    i <- NULL
    # Assigns the input of the set function to 'x' and sets the inversion
    # variable to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # Retrieves the value of the input, 'x'
    get <- function() x
    # Assigns the value of inverse to the inversion variable
    setinverse <- function(inverse) i <<- inverse
    # Retrieves the value of the inversion variable
    getinverse <- function() i
    # Generates a named list containing the four functions described above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function checks to see if the inverse of 'x' has been calculated. If it
# has, then the function returns the inverse. If not, then it calculates the
# inverse and returns it.

cacheSolve <- function(x, ...) {
    # Retrieves the value of 'inverse' and assigns it to 'i'
    i <- x$getinverse()
    # If the inverse has already been cached, it is retrieved.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # If the inverse has not already been cached, the matrix 'x' is retrieved
    data <- x$get()
    # Assigns the inverse of 'x' to 'i'
    i <- solve(data, ...)
    # Sets the inverse value to 'i' in the makeCacheMatrix list
    x$setinverse(i)
    # Returns the inverse of 'x'
    i
}
