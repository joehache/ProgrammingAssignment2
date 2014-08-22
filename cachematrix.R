## makeCacheMatrix is the object function that holds both my matrix
## and its inverse.  It has set and get functions, and get and set
## inverse functions.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(new_matrix){
                x <<- new_matrix
                inverse <<- NULL        ## Reset the inverse
        }
        get <- function() x
        set_inverse <- function(new_inverse) inverse <<- new_inverse
        get_inverse <- function() inverse
        list(set = set, get = get,
                set_inverse = set_inverse, 
                get_inverse = get_inverse)
}


## cacheSolve will solve the inverse of a matrix and store it for
## future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("Getting cached data ...")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}
