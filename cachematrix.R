## The inverse of the matrix can be an expensive operation
## The below 2 methods will cache the inverse of a matrix once it has been
## calculated and then use the cached result if it is called again.

## Create a scoped "container" to store the matrix and inverse for it,
## this will be used by the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL;

    ## store the matrix, reset the inverse_x
    set <- function(matrix) {
            x <<- matrix;
            inverse_x <<- NULL;
    }

    ## get the matrix
    get <- function() {
        x;
    }

    ## store the inverse
    setInverse <- function(inverse) {
        inverse_x <<- inverse;
    }

    ## get the inverse
    getInverse <- function() {
        inverse_x;
    }

    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse);
}


## First check if there is cached inverse available, if not calculate the inverse
## and then save it on the object (that was created using makeCacheMatrix function)
## then return the inverse.
## Assumptions: assumes first parameter is an object created using makeCacheMatrix
##              assumes that the matrix is invertible
cacheSolve <- function(x, ...) {
    ## is there a cached inverse?
    inverse_x <- x$getInverse();
    if (!is.null(inverse_x)) {
        message("using cached data for inverse");
        return (inverse_x);
    }

    ## nope, so get the inverse
    matrix <- x$get();
    inverse <- solve(matrix, ...);

    ## save / cache the inverse
    x$setInverse(inverse);
    inverse;
}
