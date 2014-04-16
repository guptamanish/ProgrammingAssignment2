##
## cacheMatrix.R
## 
## two functions:
## 1. a function to create special matrix object, capable of saving inverse of a
##    matrix along with the matrix
##
## 2. a function compute and cache matrix's inverse.
## 

##
## A constructor function, which returns an object (specialized matrix)
## which is capable of caching inverse along with the original matrix.
##
## The object provides, accessor functions to set() or get() to access the
## matrix or its inverse
##
makeCacheMatrix <- function(x = matrix())
{
    x_inv <- NULL;     ## data variable to save the inverse matrix

    set <- function(y)
    {
        x     <<- y;       ## set the data
        x_inv <<- NULL;    ## reset the calculated inverse, as the associated data has changed
    }

    get <- function()
    {
        x;  ## return the original matrix
    }

    setinverse <- function(inv) 
    {
        x_inv <<- inv;     ## set the inverse matrix
    }

    getinverse <- function()
    {
        x_inv;  ## return the inverse matrix
    }

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse);
}


##
## A function to compute an inverse of a given (non-singular) matrix.
##
## The function expects the matrix object, passed to it, to be special
## object created by makeCacheMatrix().
##
## The function makes use of the fact, that the special matrix object
## is capable of saving the inverse of the matrix along with the
## matrix itself.
## 
## The function does not compute the inverse always. It first checks
## if the object already has the inverse computed and cached. if it has
## it returns the same; if not, the function not only computes the
## inverse but also saves the same in the object before returning.
##
cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'

    x_inv <- x$getinverse();

    if ( !is.null(x_inv) )
    {
        message ("getting cached data");
        return  (x_inv);
    }

    data <- x$get();

    x_inv <- solve(data, ...);

    x$setinverse(x_inv);   ## making sure that subsequent calls don't need to compute the inverse

    return (x_inv);
}
