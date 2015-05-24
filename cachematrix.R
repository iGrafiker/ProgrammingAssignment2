## The first function "makeCacheMatrix" creates a matrix object that can cache its inverse. 
## The second function "cacheSolve" computes the inverse of the matrix returned by "makeCacheMatrix". 
## If the inverse has already been calculated then the function "cachesolve" retrieve the inverse from the cache.

## creates a matrix for testing purposes
# y <- matrix(c(-1, -2, 1, 1), 2,2)

## The function below creates four functions. 
## 1) set up a matrix and cache it
## 2) get this matrix from cache
## 3) cache the inverse of this matrix
## 4) get a already computed invers matrix from cache
## The function makeCacheMatrix returns a list containing the names of the four functions.
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
  
    #set matrix in cache
    setCache <- function(y){
        x <<- y
        inverseMatrix <<- NULL
    }
  
    #get matrix from cache
    getCache <- function(){
        x
    }
  
    #set inverse matrix in cache
    setInverse <- function(solveResult){
        inverseMatrix <<- solveResult
    }
  
    #get inverse matrix from cache
    getInverse <-function(){
        inverseMatrix
    }
    # return a list with the four named functions
    list(setCache = setCache, getCache = getCache, setInverse = setInverse, getInverse = getInverse)
}


## The function below computes the invers of the matrix provided by makeCacheMatrix. 
## If the inverse matrix has already computed and the matrix has not been changed, 
## then the function retrieve the inverse matrix from cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    inverseMatrix <- x$getInverse() # get inverse matrix from cache
    ## if no inverse matrix found in cache
    if (is.null(inverseMatrix)){ 
        data <- x$getCache() # get original matrix from cache
        solveResult <- solve(data) # compute the inverse matrix
        x$setInverse(solveResult) # set inverse matrix to cache
    }
    ## if inverse matrix was already computed
    else{ 
        message("getting cached data") 
        inverseMatrix #return inverse matrix
    }
}
