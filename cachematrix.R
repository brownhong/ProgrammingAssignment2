## makeCacheMatrix creates Firstly, the special matrix object is created by makeCacheMatrix, 
## and the inverse of the matrix should be calculated by cacheSolve
## The program would be more efficent and less time-consuming since the program will just find and return the matrix inverse
## from cashe that has been computed before without repeat calculations.

## In order to get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL    ## Assigning 'NULL' to inverse
        set_matrix <- function(y) {
             x <<- y       ## Setting the matrix 'x'
             inverse <<- NULL

}
        get_matrix <- function() x                           ## Returning matrix 'x'
        get_inverse <- function(solve) inverse <<- solve     ## Cashe the value of the inverse
        get_inverse <- function() inverse                    ## Returing inverse
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
           
 ## Otherwise, it will calculate the data inverse and set the value of the inverse in cache
 ## based on the 'set_inverse' function.


cacheSolve <- function(x, ...) {                         ## Return a matrix which is the inverse of 'x'
        inverse <- x$get_inverse()                       ## Getting inverse
        if(!is.null(inverse)) {                          ## Checking the presence of inverse
               message("getting cashed data")            ## Showing the message
               return(inverse)
               }
               data  <- x$get_matrix()                   ## Getting Matrix
               inverse <- solve(data,...)                ## Utilizing solve() to calculate inverse
               x$set_inverse(inverse)                    ## To cashe the inverse
               inverse                                   ## Returning the inverse
}
               

