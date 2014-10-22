## This function is for caching potentially time-consuming Inverse matrix  calculations

## The functions makeCacheMatrix and cacheSolve will keep an inverse matrix in cache so that
## if the same matrix needs an inverse then the cached inverse matrix is supplied else
## if new the Inverse of the matrix is calculated.

## This function will create a special matrix which will be a list containing a function
## to set the value of the matrix
## get the value of the matrix
## set the value of the Inverse matrix and
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){    ## modifies existing matrix
                 x <<- y
                 m <<- NULL
        }
        
        get <- function() x   ## returns original matrix
        setinverse <- function(inverse) m <<- inverse  
        getinverse <- function() m  ## Returns matrix inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The following function calculates inverse of a special "matrix" created with the above function
## it checks to see if an inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips computation reducing time spend on caculations. otherwise, it calculates the inverse
## of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## get the inverse matrix
        m <- x$getinverse()
        
        if(!is.null(m)){  # check to see if there is cached inverse matrix
                # if the is get the cached inverse matrix.
                
                message("getting cached inverse matrix") # message returned when getting cached inverse matrix
                return(m) # cached inverse matrix                
        }
        ## get matrix to calculate the inverse matrix
        data <- x$get()
        
        ## calculation of the inverse matrix
        m <- solve(data)   # inverse calculation of matrix
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
