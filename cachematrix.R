## The functions compute the inverse of a given matrix.
## If the inverse matrix has been computed before it is cached 
##  in the parent scope for later retrieval without having to
##  solve it again.

## makeCacheMatrix gets a matrix as argument 
## stores the matrix and the corresponding inverse matrix in parent scope
## returns a vector of functions to set and get the matrix ("set","get")
##  and to set and get the solved inverse matrix ("setsolve", "getsolve")
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverseMatrix <<- solve
        getsolve <- function() inverseMatrix
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse matrix using the built-in "solve" function
## cachesolve gets the result of the makeCacheMatrix funtion as argument
## the inverse matrix will be retrieved from parent scope 
##  if it was computed previously and stored with the help of makeCacheMatrix$setsolve
##  otherwise, it will be computed and stored now

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getsolve()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setsolve(inverseMatrix)
        inverseMatrix
}
