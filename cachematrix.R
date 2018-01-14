## store the matrix and cache the inverse

#create a special matrix to set, get the matrix and the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}





#calculate the inverse of the matrix returned by makeCacheMatrix, and if its already calculated (!is.null(m))
#return the result from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(m)
        m
}
