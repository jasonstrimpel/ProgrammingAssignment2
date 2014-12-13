## An n-by-n square matrix A is called invertible if there exists an n-by-n 
## square matrix B such that \mathbf{AB} = \mathbf{BA} = \mathbf{I}_n \  
## where In denotes the n-by-n identity matrix and the multiplication used 
## is ordinary matrix multiplication 
## (http://en.wikipedia.org/wiki/Invertible_matrix).

## Functions create a cacheable matrix object and computes the inverse while 
## caching the inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}


## Test whether a matrix A is invertible

isMatrixInvertible <- function(A) {
    rows <- nrow(A)
    eye <- diag(rows)
    B = solve(A)
    all(A %*% B == eye)
}

## Example

# create an invertible matrix
A = rbind(c(1, -1/4), c(-1/4, 1))

# simple test to confirm it's invertible 
isMatrixInvertible(A)

# get the special cacheable matrix object
cachedMatrix <- makeCacheMatrix()

# set the invertible matrix
cachedMatrix$set(A)

# since solved matrix is not cached, will not get the 'getting cached data'
# message
cacheSolve(cachedMatrix)

# since solved matrix IS cached, will get the 'getting cached data' message
cacheSolve(cachedMatrix)
