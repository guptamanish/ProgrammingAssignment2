## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
            x_inv <- NULL

            set <- function(y)
            {
                x <<- y
                x_inv <<- NULL
            }

            get <- function()
            {
                x;
            }

            setinverse <- function(inv)
            {
                x_inv <<- inv
            }

            getinverse <- function()
            {
                x_inv;
            }

            list(set = set,
                 get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Write a short comment describing this function

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

    x$setinverse(x_inv);

    return (x_inv);
}
