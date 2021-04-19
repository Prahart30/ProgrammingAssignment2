## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

 ##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize inverse property
        invm <- NULL
        #Set the matrix
        set <- function(y){
                x <<- y
                invm <<- NULL
        }
        #Get the matrix
        get <- function(){
                x
        }
        #Set the inverse of the matrix
        setInverse <- function(inverse){
                invm <<- inverse
        }
        #Get the inverse of the matrix
        getInverse <- function(){
                invm
        }
        #List of the methods
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse
}
 

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        invm <- x$getInverse()
        #Check if the matrix is alredy set
        if (!is.null(invm)){
                message("getting cached data")
                return(invm)
        }
        #Get the matrix
        data <- x$get()
        #Calculate the inverse of the matrix
        invm <- solve(data, ...)
        #Set the inverse
        x$setInverse(invm)
        #Return the inverse matrix
        invm
}
