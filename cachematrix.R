## This pair of functions are used to create and manipulate an object 
## that can internally store a matrix and its inverse, and also to cache
## and reuse the inverse if it had already been computed (and the matrix
## data did not change since then).

## This function create an object that can store a matrix and its inverse.
## It also provides methods to read and write the internally stored data
## for the matrix and for its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## ix will store the inverse of x
        ix <- NULL
        ## set or reset the matrix data
        set <- function(y) {
                ## deep assigment, modifies x in the parent environment
                x <<- y
                ## invalidate inverse when the matrix data is (re)set
                ix <<- NULL
        }
        ## get the matrix data
        get <- function() x
        ## set the inverse of the matrix
        setinv <- function(inv) ix <<- inv
        ## get the inverse of the matrix
        getinv <- function() ix
        ## provide methods to the outside world
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function is used to cache the inverse of the "cache-matrix" object define 
## in makeCacheMatrix above. If the inverse has not been computed yet, or if the 
## matrix data changed since last inverse computation, then the inverse is computed
## using the solve() method. The cached inverse matrix is instead used if available.
cacheSolve <- function(x, ...) {
        ## get the cached inverse
        inv <- x$getinv()
        ## if not NULL, then it has been already computed (and data did not change)
        if(!is.null(inv)) {
                message("getting cached data")
                ## Return a matrix that is the inverse of 'x'
                return(inv)
        }
        ## get the matrix data from the object using the get() method
        data <- x$get()
        ## use the solve() function to compute the inverse of the matrix 
        ## (no checks, we always assume that the matrix is invertible)
        inv <- solve(data)
        ## cache the data inside the object for future usage
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
