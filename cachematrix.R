## makeCacheMatrix creates a list containing a function, which
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
setmatrix <- function(y) {
x <<- y
inv <<- NULL
}
getmatrix <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}
## This following function calcukates the inverse of the special matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data")
return(inv)
}
data <- x$getmatrix()
inv <- solve(data,...)
x$setinverse(inv)
return(inv)
}
## Test my functions:
        ## >x <- rbind(c(1, 2), c(3, 4))
        ##>m <- makeCacheMatrix(x)
        ##> m$getmatrix()
        ##      [,1] [,2]
        ##[1,]    1    2
        ##[2,]    3    4
        ##> cacheSolve(m)
        ##     [,1] [,2]
        ##[1,] -2.0  1.0
        ##[2,]  1.5 -0.5
        ##> cacheSolve(m)
        ##getting cached data
        ##     [,1] [,2]
        ##[1,] -2.0  1.0
        ##[2,]  1.5 -0.5
       
