##This function creates a matrix object where its inverse can be cached. It defines argument with default mode, and initializes inv as NULL value/ 
##Then difeines the set of function to assighn new, and assighns value of the inv function

makeCacheMatrix <- function(x = matrix()) {  
    inv <- NULL                              
    set <- function(y) {                    
        x <<- y                              
        inv <<- NULL                         
    }
    get <- function() x                     

    setinverse <- function(inverse) inv <<- inverse   
    getinverse <- function() inv                      
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
                                                                                   
}

##This function returned by makeCacheMatrix calculates the inverse matrix, 
# If the inversion has already been calculated (and the matrix has not changed), it retrieves the inversion from the cacheSolve

cacheSolve <- function(x, ...) {
         
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
