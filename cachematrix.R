## These functions create a special matrix object containing four functions
## used for storing and retrieving the original matrix and its inversion.
## And also calculate the inversion using the special matrix object.
##
## Usage:
## \code{m <= makeCacheMatrix(x)} - stores the special matrix object
## \code{cacheSolve(m)} - returns the cache inversion of the matrix
##
## The speed can be checked with this code:
## \code {
## n <- 5000
## matrix <- matrix(runif(n^2), nrow=n, ncol=n)
## m <- makeCacheMatrix(matrix)
## system.time(inverted <- cacheSolve(m))
## system.time(inverted <- cacheSolve(m))
## }
##
## On my machine the first call takes 26s, all next 0s.


#' Creates a special version of a "matrix" object which can cache its inversion.
#'
#' @param x A matrix object
#'
#' @return A list containing four functions:
#'          get - stores the matrix
#'          set - returns the matrix
#'          getInverse - returns the inverted matrix
#'          setInverse - stores the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    # the cached matrix
    m <- NULL

    # a function for storing the matrix value,
    # also clears the old inverted matrix
    set <- function(value) {
        x <<- value
        m <<- NULL
    }

    # function for getting the matrix value
    get <- function() x

    # function for storing the inverted matrix
    setInverted <- function(solve) m <<- solve

    # function for getting the inverted matrix
    getInverted <- function() m

    # the final list to be returned
    list(get = get,
         set = set,
         getInverted = getInverted,
         setInverted = setInverted)
}


##' Returns the inverse of the matrix x.
##'
##' The function uses the input special matrix to cache the inverted value,
##' so the first run can be long, depending on the machine and the matrix size.
##' The next runs should return the cached value in almost no time.
##'
##' @param x A matrix object created with makeCacheMatrix
##'
##' @return The inverse of the input matrix x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverted()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverted(m)
    m
}
