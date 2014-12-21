## The two functions below are as required for the assignment. The first function creates and object
## that can store a matrix and also cache it's inverse. 

## makeCacheMatrix lets user initiate an object that stores a matrix and it's inverse.
## When initiated, the function returns a list that contains four functions.
## The four functions are:
## 1. get - this retrieves the matrix provided by the user
## 2. set - this function stores the matrix in the object and clears the value of the inverse of 
##      previously stored matrix, if there was one
## 3. getinverse - this function retrieves the value of the inverse
## 4. setinverse - this function stores the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        
        set <- function(input) {
                x <<- input
                inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(computedinverse) inverse <<- computedinverse
        
        getinverse <- function() inverse
        
        list (set = set, 
              get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)

}


## cacheSolve returns the inverse of the matrix it receives as input and returns it's
## inverse. If the inverse is already available in cache, it returns the value. If the 
## cache does not have inverse, it computes one and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("Getting cached value of inverse")
                return(inv)
        }
        
        data <- x$get()
        computedinverse <- solve(data, ...)
        x$setinverse(computedinverse)
        
        computedinverse
}
