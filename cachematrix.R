##############################################################################
## Script Name - cachematrix.R
## This script contains functions which creates and caches the inverse
## of a given matrix. If the matrix is a new one, its inverse is calculated
## by using the solve() function and the output is cached. If the inverse is
## required for a matrix for an existing matrix whose inverse was already
## calculated before, the cached value is retrieved instead of a fresh
## calculation.
##############################################################################

## Function - makeCacheMatrix()
## This function creates a special "matrix" data structure and creates a list
## of functions. These functions are:
##     1. set_matrix - Sets the matrix into the structure.
##     2. get_matrix - Retrieves the matrix from the data structure.
##     3. set_inverse - Saves the inverse of the matrix (Using solve())
##     4. get_inverse - Retrieves the inverse of the matrix
##
## Sample Usage:
##     mat <- matrix(rnorm(16), 4, 4) ; Creates the 4x4 matrix "mat"
##     special_mat <- makeCacheMatrix() ; Initializes the special matrix
##     special_mat$set_matrix(mat) ; Saves the "mat" to the special matrix
##     special_mat$get_matrix() ; Retrieves the value of the "mat"
##     special_mat$get_inverse() ; Retrieves the inverse of "mat"

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set_matrix <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(solve) m_inverse <<- solve
        get_inverse <- function() m_inverse
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## Function - cacheSolve()
## This function calculates the inverse of the special matrix object created
## using the makeCacheMatrix by running the solve() function.
## If the function is executed for the first time, the inverse is calculated
## and saved to cache.
## If the function is executed subsequently, the cached value is directly
## retriecved.
##
## Sample Usage:
##    cacheSolve(special_mat) ; Calculates the inverse, either calculates or
##                              retrieves from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$get_inverse()
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        data <- x$get_matrix()
        m_inverse <- solve(data, ...)
        x$set_inverse(m_inverse)
        m_inverse
}
