
## Write a short comment describing this function
## Create a matrix object that cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL 
                
        }
        ##setting / intializing needed fields
        get <- function() X
        setinverse <- function(inverse)i<<- inverse
        getinverse <- function()i
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function
## Caluclulate the inverse of matrix.
## Check to see if caclulation is needed? i.e.: Is the value already stored
## If required CacheSolve will retrieve inverse from Cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<x$getinverse()
        if(!is.null(i)){
                message("retrieveing cached data")
                return(i)
        }
        ## Calculation if not already cached
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
        
}
