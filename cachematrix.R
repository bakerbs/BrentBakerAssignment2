## makeCacheMatrix creates a 'special' matrix, x, and allows a 
## user(or another function) to get and set the elements of the matrix
## as well as the matrix's inverse.  The inverse is NULL by default and a new
## 'setting' of the elements of a matrix resets that matrix's inverse to NULL.

## cacheSolve uses the embedded functions within makeCacheMatrix to evaluate 
## and set the corresponding inverse to a matrix defined in makeCacheMatrix.
## cacheSolve first checks to see if the matrix's inverse has previously been
## defined.  If it has, cacheSolve will return the stored matrix inverse, 
## saving calculation time.  If a particular matrix's inverse has not been
## defined, cacheSolve will use the 'solve()' function to solve for the 
## matrix's inverse. Unless otherwise defined, cacheSolve assumes that the 
## corresponding 'solve' matrix is the identity matrix of appropriate size. 


## makeCacheMatrix creates a list containing 4 matrix
## functions: set, get, setinv, and getinv. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y         ## <<- assigns y to x outside of this function
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## cacheSolve either retreives a previously stored matrix inverse (if one is
## available) or solves for a matrix's inverse. 

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        i <- x$get()
        inv <- solve(i, ...)
        x$setinv(inv)
        inv
}
