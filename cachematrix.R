## makeCacheMatrix:
## To create a special cached matrix object call the function
## "makemakeCacheMatrix" with no arguments, like:
## x <- makeCacheMatrix()
## and use the "set" fuction associated with the
## object "x" to define the matrix later, or
## call the function "makemakeCacheMatrix" with a matrix
## argument, like : x <- makeCacheMatrix(matrix(1:4, 2, 2))
## and use x$get() to get the value of the martix or use the
## "set" fuction associated with the object "x" to re-define
## the matrix later.
## To initiate a matrix call the function "set" associated with 
## the special cached matrix object.
## For instance, x$set(matrix(1:4, 2, 2))
## To get the value of the special cached matrix object call
## the function "get" associated with the object.
## For instance, mat <- x$get()
## To set the value of the inverse of the special cached matrix
## object call the function "setinv" associated with the object.
## For instance, x$setinv(matrix(c(-2, 1, 1.5, 5), 2, 2))
## To get the value of inverse of the special cached matrix
## object call the function "getinv" associated with the object.
## For instance, invmat <- x$getinv()

## cacheSolve:
## To calculate the the inverse matrix of the matrix
## stored into the cache call the function "cacheSolve"
## with the special cached matrix object as argument.
## For instance, y <- cacheSolve(x)
## The function first checks to see if the inverse has already been
## calculated and stored into the cache.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the "setinv" function
## associated with the object (here, "x").


## This functon returns a list of functions: set, get, setinv
## and getinv to set and get the matrix and to set and get 
## the inverse matrix respectively
makeCacheMatrix <- function(x = matrix()) {
        ## x is a matrix.
        
        inv <- NULL
        set <- function(y) {
                ## y is a matrix.
                
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                
                x
        }
        setinv <- function(inverse) {
                ## inverse is the inverse matrix of the special cached matrix
                ## object.
                
                inv <<- inverse
        }
        getinv <- function() {
                
                inv
        } 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function returns the inverse matrix of the special
## cached matrix object created by the function "makemakeCacheMatrix"
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
