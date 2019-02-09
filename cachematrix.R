## This is a special matrix inverse calculation that stores previously
## calculated results in cache for future reference

## Creates a matrix and sets its inverse

makeCacheMatrix <- function(x = matrix()) {
    matinv = NULL
    set <- function(y){
        x <<- y
        matinv <<- NULL
    }
    get <- function() x
    setinv <- function(matinverse) matinv <<-matinverse
    getinv <- function() matinv
    list(set = set,get = get, setinv = setinv, getinv = getinv)
}


## Returns inverse of matrix from cache . If not available then calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matinv <- x$getinv()
    if(!is.null(matinv)){
        message('Getting data from cache.')
        return(matinv)
    }
    data <- x$get()
    matinv <- solve(data,...)
    x$setinv(matinv)
    matinv
  
}