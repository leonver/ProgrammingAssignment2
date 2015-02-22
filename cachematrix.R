## The primary goal of these functions is to create the inverse matrix of square matrix.
## for optimization reason the inverse will only be calculated the first time it's offered
## for approaching the inverse matrix after the first time the stored result will be returned 
## The inverse matrix is created with the "solve()" function
## This solution assumes matrix supplied will always be square matrices and allways invertible

## This function Creates a list with actions to be executed for the calcultion of the optimized 
## calculation of the inverse matrix  

makeCacheMatrix <- function(x = matrix()) {
    cinv <- NULL                         # placehoder for the calculated inverse matrix
    set <- function(nM) {                # function to provide the makeCacheMatrix with new values 
        x <<- nM                         # for a new, tocalcalute, inverse matrix.      
        cinv <<- NULL                    # The value for the placeholder of the inverse matrix
                                         # must be set to null, because with new matrix values
                                         # the inverse matrix has to be recalculated
        
    }
    getm <- function() x                 # returns the input matrix
    setinvm <- function(invm) cinv <<- invm # sets the placeholder to the value of the inverse matrix
    getinvm <- function()  cinv          # returns the value of the placeholder for the inverse matrix 
    list(set = set, getm = getm,         # makes the defined actions available
         setinvm = setinvm,
         getinvm = getinvm)
    
}


## function to get the inverse matrix of a given matrix. When this Inverse matrix is already
## calculated it will return the inverse matrix from the placeholder. If not the inverse matrix
## is calculated with the solve function and stored in the placeholder.

cacheSolve <- function(x, ...) {
    linv <- x$getinvm()          # get the value of the placeholder of the inverse matrix
    if(!is.null(linv)) {         # check to see if the placeholder has a value for the inverse matrix
        message("The inverse value of the matrix is returned without recalculation")
        return(linv)             # returns value of the placeholder and ends function
    }
    message("The inverse value of the matrix had to calculated")
    lmatrix <- x$getm()          # the value of the placeholder is null and will now be calculated 
    linv <- solve(lmatrix, ...)  # Calculates the inverse matrix
    x$setinvm(linv)              # Give the placeholder the value of the inverse matrix
    linv                         # return the result for the matrix
}

## test script for function
#test <- makeCacheMatrix(matrix(runif(16,1,100),4,4)) # create object with a 4 * 4 matrix
#testsolve <- cacheSolve(test)                        #return the calculated inverse matrix
#test$getm()                                          #return the original matrix
#testsolve <- cacheSolve(test)                        #return the inverse matrix without calculation
#testsolve                                            #write the inverse matrix to the console 
#testsolve <- cacheSolve(test)                        #return the inverse matrix without calculation
#cacheSolve(test)                                     #writes the inverse matrix direct to the console
#test$set(matrix(runif(25,1,200),5,5))                #set matrix to a new value
#testsolve <- cacheSolve(test)                        #return the calculated inverse matrix
#test$getm()                                          #return the original matrix
#testsolve <- cacheSolve(test)                        #return the inverse matrix without calculation
#testsolve                                            #write the inverse matrix to the console 
#testsolve <- cacheSolve(test)                        #return the inverse matrix without calculation

