## Below are 2 fuctions for caching the inverse of a matrix

## Function 1: Creates a special matrix object that:
## 1.1. Sets the matrix
## 1.2. Gets the matrix
## 1.3. Sets the inverse of the matrix
## 1.4. Gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                m <<- inverse
        } 
        getInverse <- function() {
                m
        } 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Function 2: The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        else {
                message("No cached matrix. Might take time to calculate Inverse of the matrix")
                data <- x$get()
                m <- solve(data)
                x$setInverse(m)
                return(m)
        }
}
