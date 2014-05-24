# The function makeCacheMatrix defines the general properties of a "special" matrix. 
# The two main attributes of this object are x which is a matrix and "m" which 
# is supposed to store the inverse of such a matrix (using the function cacheSolve) 
# and has a default value equal to NULL. The subfunctions of 
# makeCacheMatrix are the following:
# set makes attaches to x the value of y
# get just get the object x
# setinverse caches the value of the inverse in the object by assigning a value to "m"
# getinverse get the value of "m"

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# The function cacheSolve returns the inverse of an object of the type defined by makeCacheMatrix. Before computing the inverse the function
# checks whether the inverse ("m") has already been stored by using the subfunction of the object "getinverse". If the returned
# object has been retrieved from cached data the function displays the message
# "getting cached data". If the function does not find a value attached to "m" it computes the inverse of the matrix
# and stores it by rewriting "m" using the function setinverse.

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

# i am sorry if i don't use the appropriate programming language while commenting but i am new to this stuff. I hope 
# i did it in the correct way. 




