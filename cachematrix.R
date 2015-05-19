## makeCacheMatrix and cacheSolve functions will inverse a matrix and cache the inversed matirx in a list.
## runCacheMatrix tests the makeCacheMatrix and cacheSolve functions


## makeCacheMatrix creates a special "matrix" (a list) containing functions to:
#### 1.set the value of the matrix (get)
#### 2.get the value of the matrix (set)
#### 3.set the value of the inversed matrix (setinverse) 
#### 4.get the value of the inversed matrix (getinverse) 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    # return the list
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## cacheSolve will calculates and return the inverse of the special "matrix" (a list).
## If the inverse has already been calculated, it gets the inversed matrix from the cache and skips the computation.
## Otherwise, it calculates the inversed matrix and sets the value from the computation.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        ## Check if the inverse of 'x' has been cached
        i <- x$getinverse()        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## Calculate the inverse of 'x', cache the inverse, and return it.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


## runCacheMatrix tests the makeCacheMatrix and cacheSolve functions with a 2x2 matrix and 8x8 matrix 
runCacheMatrix <- function() {
  #create a sqaure matrix 2 by 2 (2x2)
  h2<-rbind(c(1, -1/4), c(-1/4, 1))
  message("2x2 matrix")
  print(h2)

  #create a sqaure matrix 8 by 8 (8x8)
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  h8 <- hilbert(8)
  message("8x8 matrix")
  print(h8)
  
  
  #cache the h2 (2x2)
  message("inverse and cache 2x2 matrix")
  h2Cache<-makeCacheMatrix(h2)

  #Run cacheSolve twice; first call caches the value; second call returns the cached value
  cacheSolve(h2Cache)
  print(cacheSolve(h2Cache))


  #cache the h8 (8x8)
  message("inverse and cache 8x8 matrix")
  h8Cache<-makeCacheMatrix(h8)
  
  #Run cacheSolve twice; first call caches the value; second call returns the cached value
  cacheSolve(h8Cache)
  print(cacheSolve(h8Cache))
  
  
  # Confirm the matrix is an Identify Matirx
  message("2x2 Identify Matirx")
  print(round(h2Cache$getinverse() %*% h2Cache$get(), 3))
  message("8x8 Identify Matirx")
  print(round(h8Cache$getinverse() %*% h8Cache$get(), 3))
}