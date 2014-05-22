## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # set null value
    set <- function(y) { # set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x  # get the value of the matrix
    setinverse <- function(inverse) inv <<- inverse # set the value of the inverse
    getinverse <- function() inv  # get the value of the inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # listing the values
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    
    #getting value from cache
    inv <- x$getinverse() # get value from cache
    if(!is.null(inv)) { # if not null
        message("getting cached data.") #display message
              return(inv) # return value
    }
    #value is not in cache, so calculate
    data <- x$get() # get matrix from the cache
    inv <- solve(data) # this will inverse the square matrix
    x$setinverse(inv) # set calculated value to cache
    inv # return value
}
