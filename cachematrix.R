## makeCacheMatrix computes the matrix inverse of matrix X
## cacheSolve checks to see if a solution for X has already been computed
## and, computes and displays it if not; otherwise it displays the
## previously computed result

## Caching the solution to the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Check if matrix inverse is cached; solve if not

cacheSolve <- function(x, ...) {
           m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

