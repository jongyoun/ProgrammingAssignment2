## Put comments here that give an overall description of what your
## functions do

## makeCascheMatrix() 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set_mtx <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get_mtx <- function() x
  setInv <- function(invers) invrs <<- invers
  getInv <- function() invrs
  list(set_mtx = set_mtx, get_mtx = get_mtx,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInv()
  if (!is.null(invrs)) {
    message("getting cached inverse matrix")
    return(invrs)
  }
  data <- x$get_mtx()
  invrs <- solve(data, ...)
  x$setInv(invrs)
  invrs
}

## Let me show you how to test two functions above
## # First, making a sample object by using "makeCascheMatrix" function
## > test <- makeCacheMatrix() 

## # Then, set any matrix that has its inverse
## > test$set_mtx(matrix(c(3,2,0,0,0,1,2,-2,1),3,3))

## # You can test by typing "test$get_mtx()" to get 
## > test$get_mtx()
##       [,1] [,2] [,3]
## [1,]    3    0    2
## [2,]    2    0   -2
## [3,]    0    1    1

## # Now, you're ready to get the inverse matrix by using cacheSolve(test)
## > cacheSolve(test)
##       [,1] [,2] [,3]
## [1,]  0.2  0.2    0
## [2,] -0.2  0.3    1
## [3,]  0.2 -0.3    0
## # You don't get the message "getting cached inverse matrix", 
## # which implicates that the result came through.

## # Now if you try one more,
## > cacheSolve(test)
## getting cached inverse matrix
##       [,1] [,2] [,3]
## [1,]  0.2  0.2    0
## [2,] -0.2  0.3    1
## [3,]  0.2 -0.3    0

