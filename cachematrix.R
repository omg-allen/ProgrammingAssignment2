## Student: Al Shain
## Email: al_shain@me.com
## Programming Assignment 2: Lexical Scoping 
## Description: 
## Write the following functions:
##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a 
## square invertible matrix, then solve(X) returns its inverse.
##
## For this assignment, assume that the matrix supplied is always invertible.

## makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix()) {
  ## initialize & set cache to NULL
  cache <- NULL
  
  ## create the matrix   
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ## get values x of the matrix
  get <- function() x
  ## store the inverse of the matrix to cache
  setInverse <- function(inverse) cache <<- inverse 
  ## get the inverted matrix from cache
  getInverse <- function() cache
  ## return the function to the environment
  list(set=set, 
       get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}


## cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## set the cache to the inverse matrix
  cache <- x$getInverse()
  ## test to make sure we have something in the cache, if we do return the inverted matrix otherwise we'll need to create it
  if (!is.null(cache)){
    return(cache)
  }
  ## we didn't find anything in the cache, hence NULL, so we need to create it
  matrix <- x$get()
  ## set the cache to the matrix
  cache <- solve(matrix, ...)
  ## now invert the matrix
  x$setInverse(cache)
  ## return the inverted matrix
  return(cache)
}
