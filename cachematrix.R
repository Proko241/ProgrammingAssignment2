##These functions allow you to set a matrix and calculated the invers.  If the 
##inverse has already been calculated it will not be recalcualted, but pulled

## set the value of the matrix 
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setminv <- function(inv) minv <<- inv
        getminv <- function() minv
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}


## Calculates the inverst of the matrix created in makeCacheMatrix if it 
## has not already been calculated

cacheSolve <- function(x, ...) {
        minv <- x$getminv()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
        x$setminv(minv)
        minv
}

