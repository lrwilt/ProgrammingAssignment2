## makeCacheMatrix caches the inverse of a matrix by storing the initial matrix
## in variable x and then creates four closure functions that can house and
## return the inverse matrix created by cachSolve. makeCacheMatrix returns these
## four closure functions as a list that can be accessed by cacheSolve.

## cacheSolve first checks for a cache of the inverse matrix within the
## instance of makeCacheMatrix. If one is strored, that is returned. If not,
## cacheSolve retrieves the intial matrix stored in variable x of 
## makeCacheMatrix and calculates the inverse. Then cacheSolve stores the inverse
## of the matrix in the setinverse function within makeCacheMatrix.


startMatrix <- matrix(1:4, 2)

## Store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set <- function(y) { ##allows for modifying the starting function
        x <<- y
        m <<- NULL
    }
    get <- function() x ## returns the value of x to cacheSolve
    setinverse <- function(solve) m <<- solve ## used by cacheSolve to set m as the inverse of the matrix stored in x
    getinverse <- function() m ## returns the value of the inverse matrix stored by cachSolve in m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ##stores and returns the functions as a list,
                                                                                 ##which can be accessed by cacheSolve
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get() ##getting the begining matrix, which is stored in x
                        ##inside of the instantiated instance of the makeCachMatrix environment
        m <- solve(data) ##creates the inverse
        x$setinverse(m) ##cache the inverse back in the instantiated instance of the makeCachMatrix environment
        m
}
