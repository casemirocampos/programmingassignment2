## This function is capable of inverting a matrix in a way that, if the progam has
## already inverted the same matrix, the function wil be able to use the result. 
## Otherwise she will just calculate.

## This first function ("makeCacheMatrix) will creat a matrix that is capable of
## cache its inverse


makeCacheMatrix <- function(x = matrix()) {
                  m<-NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setmatrix <- function(inverse) m <<- inverse
                getmatrix <- function() m
                list(set = set,get = get,
                     setmatrix = setmatrix,
                     getmatrix = getmatrix)
}

## Now, the function "cacheSolve" will use this matrix that is capable of cache 
## its inverse and will calculate the inverse, if this problem has not been done.

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix<- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
