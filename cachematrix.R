#### Matrix inversion is usually a costly computation and there may be some benefit to caching 
#### the inverse of a matrix rather than compute it repeatedly (there are also alternatives to 
#### matrix inversion that we will not discuss here). Your assignment is to write a pair of 
#### functions that cache the inverse of a matrix.

#### Write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

#### Function Description
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverser <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverser = getinverser)
}


#### Function description
## The following function returns cached inverse matrix, 
## if inverse isn't cached, compute inverse and cache it. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
} 
