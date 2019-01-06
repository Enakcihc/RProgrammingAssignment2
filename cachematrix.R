## These are two complementary functions which stores and
## cache's the inverse of a (presumably invertible) matrix
## 

##  makeCacheMatrix(x) creates a special list to store a matrix
##  with value x, and contains a cache of the inverse of the 
##  matrix, if already calculated. It also defines a set of 
##  functions to set and retrieve the values of the original 
##  matrix or the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # initializes inv in the makeCacheMatrix scope for storing the inverse matrix
    set <- function(y) { # changes value of the matrix, and reset the stored inverse to NULL
        x <<- y
        inv <<- NULL
    }
    get <- function() x # returns the value of the matrix
    setinv <- function(inverse) inv <<- inverse # store the value of the inverse (e.g. after calculated by cacheSolve)
    getinv <- function() inv # returns the value of the stored inverse matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##  cacheSolve(x) calculates the inverse of matrix x (created by 
##  makeCacheMatrix) and store it in the cache (inv), or in case
##  the inverse has already been calculated retrieves it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) { ## if an inverse matrix exists
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data) ## calculates the inverse matrix
    x$setinv(inv)
    inv
}
