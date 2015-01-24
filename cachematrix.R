## The functions here allow you to cache the inverse of a matrix for later use.
## Function makeCacheMatrix allow you to create an object that will allow you to
## achieve the above. 
## Function "cacheSolve" will allow you to get the inverse of a matrix stored in 
## a "CachedMatrix" object and it is able to cache the this inverse.

## Use should use these functions as follow,
## 1.Create a raw matrix whose inverse you wish to be cached, for example
##    c2<-matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## 2.Create an "CachedMatrix" object using for example,
##    cm2<-makeCacheMatrix()
## 3.Set the matrix data for this created object, for example,
##    cm2$set(c2)
## 4.Find the cached inverse of your data matrix by,
##    cacheSolve(cm2)


## This function creates an empty "CacheMatrix" object with methods which
## you can use to set/get the underlying matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function returns the inverse of the underlying matrix for the passed in 
## "CacheMatrix" object. It would return the cached inverse if available else it will
## calculate it and cache it.

cacheSolve <- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
}
