## Matrix inversion would be a costly computation and thus it is beneficial if 
## the inverse of a matrix only has to be computed once, then cached to avoid
## having to repeatedly compute the inverse of that matrix.


## This function creates a list as output. The function allows to 
## set the matrix, change the vector stored in the main function,
## store the value of input into the main function (setinverse) and return it (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## The function below checks whether the inverse of the matrix has been computed; 
## if yes, then it will return the inverse and give a message: "Getting cached data". 
## If not, the function computes the inverse (using the solve() function), sets
## the resulting value in cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message ("getting cached data")
                return (inv)
        }
        data <- x$get
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

        