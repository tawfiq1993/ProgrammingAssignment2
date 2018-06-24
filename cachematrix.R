## The followig program is for calculating inverse of matrix
## Inverse is to be stored in cache for reducing runtime 

## makeCacheMatrix() <- assign default values to arguments
## Create a list of arguments to be stored in the directory

makeCacheMatrix <- function(x = matrix()) {

    matrix_inverse <- NULL ##initializing default value of matrix inverse to NULL
    
##Set_matrix function used to change value of the assigned without creating
##another instance of the object
    
    set_matrix <- function(y){    
        x <<- y
        matrix_inverse <<-NULL
    }
    
    get_matrix <- function() x  
    set_inverse <- function(m) matrix_inverse <<-m
    get_inverse <- function() matrix_inverse
    
    list(set_matrix = set_matrix, set_inverse = set_inverse, get_matrix = get_matrix,
         get_inverse = get_inverse) 
}


## cacheSolve() <- function to generate inverse of the matrix 
## Store the matrix created in the directory

cacheSolve <- function(x, ...) {
        ## To check if the matrix is stored in the directory
    if(!is.null(x$get_inverse())) {
        message("Retrieved from the cached directory")
        return(x$get_inverse())
    }
    
    new_matrix <- x$get_matrix()
    matrix_inverse <- solve(new_matrix) ##Generate matrix inverse
    x$set_inverse(matrix_inverse)
    matrix_inverse

}
