## The function makeCacheMatrix is a list of four functions. 
# set changes the value of x in y
# get just get the object x
# setinverse solves the matrix x
# getinverse get the value of the inverse of x

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<- NULL
        }

        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve returns the inverse of x. Before computing the inverse
# checks whether the inverse ("m") has already been stored. If the returned
# object has been retrieved from cached data the function displays the message
# "getting cached data"

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        A <- x$get()
        m <- solve(A, ...)
        x$setinverse(m)
        m
}




