## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

runCacheMatrix <- function() {
  #create a sqaure matrix 2 to the 2nd power (2^2)
  h4<-rbind(c(1, -1/4), c(-1/4, 1))
  message("2^2 matrix")
  print(h4)

  #create a sqaure matrix 2 to the 3rd power (2^3)
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  h8 <- hilbert(8)
  message("2^3 matrix")
  print(h8)
  
  
  #cache the h4 (2^2)
  message("inverse and cache 2^2 matrix")
  h4Cache<-makeCacheMatrix(h4)

  #Run cacheSolve twice; first call caches the value; second call returns the cached value
  cacheSolve(h4Cache)
  print(cacheSolve(h4Cache))


  #cache the h8 (2^3)
  message("inverse and cache 2^3 matrix")
  h8Cache<-makeCacheMatrix(h8)
  
  #Run cacheSolve twice; first call caches the value; second call returns the cached value
  cacheSolve(h8Cache)
  print(cacheSolve(h8Cache))
  
}