## The following two functions will provide an option to create a Matrix 
## that allows to cache the computation of the inverse of itself. This is 
## useful since the generation of the inverse using the solve() command can
## take a long time for large matrices.

## This function creates a "special" matrix. It contains a list that allows to
## (1.) set the value of the matrix, (2.) get the value of the matrix, 
## (3.) set the value of the inverse of the matrix, (4.) get the value of the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    set.solve <- function(solve) s <<- solve
    get.solve <- function() s
    list(set = set, get = get, set.solve = set.solve, 
         get.solve = get.solve)
}


## This function calculates and caches the inverse of the "special" matrix, 
## if it hasn't been cached before. At the end it returns the inverse of the 
## matrix.

cacheSolve <- function(x, ...) {
    s <- x$get.solve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$set.solve(s)
    s
}
