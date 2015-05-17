## makeCacheMatrix  and cacheSolve functions are used to create a
## special object that stores a matrix and cache the inverse of the matrix.
## The functions are based on the assumption that the input matrix
## is always invertible.

## The following function takes a matrix as input and returns a list
## which contains functiona to set the value of the matrix, get the
## value of the matrix, get the value of inverse of the matrix and
## set the value of inverse of the matrix.

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


## The following function takes the output of the previous function (makeCacheMatrix) 
## as its input. If the value of inverse already exists in cache, then the cached value is fetched
## and returned. If value of inverse does not exist in the cache, the inverse for the
## matrix which was given as input to the function makeCacheMatrix is calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        input <- x$get()
        
        m <- solve(input, ...)
        
        x$setinverse(m)
        m
}
