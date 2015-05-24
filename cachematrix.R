## makeCacheMatrix  and cacheSolve functions are used to create a
## special object that stores a matrix and cache the inverse of the matrix.
## The functions are based on the assumption that the input matrix
## is always invertible.

## The following function takes a matrix as input and returns a list
## which contains functions to set the value of the matrix (set function), 
## get the value of the matrix (get function), get the value of inverse of 
## the matrix (getinverse function) and set the value of inverse of the matrix (setinverse function).
## The <<- operator has been used here to assign the value of inverse to the object inv
## which exists in a different environment than the current environment.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function(){
                x
        }
        
        setinverse <- function(inverse){
                inv <<- inverse
        }
        
        getinverse <- function(){
                inv
        }
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function takes the output of the previous function (makeCacheMatrix) which is a special matrix
## as its input. It first checks if the value of inverse already exists in cache. If it exists the cached value is 
##fetched and returned by the function. If the value of inverse does not exist in the cache, the inverse for the
## matrix which was given as input to the function makeCacheMatrix is calculated using the solve function and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        input <- x$get()
        
        #compute the inverse of input using solve function
        m <- solve(input, ...)
        
        #cache the matrix inverse
        x$setinverse(m)
        
        m
}
