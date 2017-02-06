## Coursera Assignment - Week 3
## Author Regina Zepeda-Albexon
## Date: 05 February 2017
## Assignment: Caching the Inverse of a Matrix
## Function Name : makeCacheMatrix
## Functionality: This is one of two functions which will cache the inverse of a matrix
## Purpose : Caching the inverse of a matrix will save a good amount of computational time. Caching the results
##           of operations which have been previously calculated will impede repeated computation of the same operations.
##
## Function 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
## 1. set the value of the matrix
   set <- function(y) {
     x <<- y
     m <<- NULL
    }

## 2. get the value of the matrix
   get <- function() x

   ## 3. set the value of the inverse of the matrix
   setMatrixInverse<- function(matrixInverse) m <<- matrixInverse

   ## 4. get the value of the inverse of the matrix
   getMatrixInverse <- function() m
   list(set = set, get = get,
        setMatrixInverse = setMatrixInverse,
        getMatrixInverse = getMatrixInverse)
   }
  
## Function 2 : cacheSolve
## Functionality:
## a. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## b. If the inverse has already been calculated and the matrix has not changed, the inverse of the matrix will 
##    be retrieved from the cached values which were previously computated
##
    
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrixInverse()
    
    ## If the matrix is not null, get the cached data
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
        data <- x$get()
               
      ## Computing the inverse of a square matrix can be done with the solve function in R.
      ## Note: If X is a square invertible matrix, then solve(X) returns its inverse.
      ## For this assignment, it will be assumed that the matrix supplied is always invertible.
          
       #m <- solve(data, ...)
       m <- solve(data)
       x$setMatrixInverse(m)
       m
      }

 ## 1. First test with matrix 1 (m1) inputing a matrix with values 1,2,3,4 
 ## > m1 <- matrix(c(1,2,3,4),nrow=2, ncol=2)
 ## > mtest <- makeCacheMatrix(m1)
 ## > cacheSolve(mtest)
 ## [,1] [,2]
 ## [1,]   -2  1.5
 ## [2,]    1 -0.5
 ## > cacheSolve(mtest)
 ## getting cached data
 ## [,1] [,2]
 ## [1,]   -2  1.5
 ## [2,]    1 -0.5
  

 ### 2. Second test with matrix 2 (m2) inputing a matrix with values of the first tests output(-2,1,1.5,-0.5)
 ##   in order to test the inversion of the first test 
 ##
 ## > m2 <- matrix(c(-2,1,1.5,-0.5),nrow=2, ncol=2)
 ## > mtest2 <- makeCacheMatrix(m2)
 ## > cacheSolve(mtest2)
 ## [,1] [,2]
 ## [1,]    1    3
 ## [2,]    2    4
 ## > cacheSolve(mtest2)
 ## getting cached data
 ## [,1] [,2]
 ## [1,]    1    3
 ## [2,]    2    4
 ## > 
 ## The test was successful :)