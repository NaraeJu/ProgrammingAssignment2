
## Overall description
## Two functions, makeCacheMatrix and cacheSolve, will allow to store an inverted matrix in a cache and
## retrieve it without cumbersome comcupatition with the same matrix. 
## MakeCacheMatrix will create an object that stores a matrix and its inverted. 
## cacheSolve will retrieve the inverted matrix from the cache that is stored in MakeCacheMatrix() envrionemnt. 


## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { ## set the value of a matrix
          x <<- y
          i <<- NULL
        }
        get <- function() x ## get the value of a matrix
        setinverse <- function(solve) i <<- solve ## set the value of the inverse
        getinverse <- function() i ## get the value of the inverse
        list(set = set, get = get,  ## creates a list containing set function, get function, 
             setinverse = setinverse, ## setinverse function, and getinverse function
             getinverse = getinverse)
}


## Compute the invere of the matrix returned by makeCacheMatrix()
## if the inverse has already calculated, then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse() ## Return a matrix that is the inverse of 'x'
        if(!is.null(i)) { ## Check the inverse has already calcualted 
          message("getting cached data") ## If so, get the inverse from the cache
          return(i)
        }
        data <- x$get() ## If not, get the value of a matrix
        i <- solve(data, ...) ## calculate the inverse
        x$setinverse(i) ## set the value of the inverse in the cache
        i ## print inverse
}

# Check whether functions work
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
CacheMatrix_object <-  makeCacheMatrix(m1)
CacheMatrix_object$get() # retrieve the value of x
CacheMatrix_object$getinverse() # retrieve the value of i, which should be NULL
cacheSolve(CacheMatrix_object) # retrieve the inverse of the matrix
CacheMatrix_object$getinverse() # retrieve the inverse directly, now that it has been cached 
