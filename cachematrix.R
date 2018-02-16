## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y # set the value of x in cache
        i <<- NULL #initialize value of i to NULL
    }
    get <- function() x #return value of x from cache
    setinverse <- function(inverse) {
        i <<- inverse #set value of i in cache
    }
    getinverse <- function() i #return value of i from cache
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse) # return a list with all the functions
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If inverse has already been calculated, then inverse is retrieved from the cache
cacheSolve <- function(x = matrix(), ...) {
    i <- x$getinverse() #try to read inverse from cache
    if(!is.null(i)) { #i is not NULL; this means that inverse is found in cache
        message("getting cached data") #display message that data being is retrieved from cache
        return(i) #return inverse from cache
    }
    data <- x$get() #this means inverse is not found in cache, so set data to value of x from cache
    i <- solve(data) #calculate inverse
    x$setinverse(i) #set value of inverse in the cache
    i #return value of inverse from cache
}
