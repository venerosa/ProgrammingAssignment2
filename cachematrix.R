## Programming Assignement 2, Catching the inverse of a Matrix:
## Since the calculation of the inverse of a matrix is very expensive
## computationally talking, catching the inverse could be a good alternative
## specially for the repeating calculation of the inverse.

## makeCacheMatrix is a function that create a list containing the functions
## to catche a matrix and its inverse. The input x is the matrix to catche.

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) I <<- inverse
      getinverse <- function() I
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## cacheSolve calculates and catches the inverse of a matrix, if it is
## already calculated it uses the inverse stocked and if is not calculated
## it calculate and stock the matrix

cacheSolve <- function(x,...) {
      I <- x$getinverse()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      data <- x$get()
      I <- solve(data,...)
      x$setinverse(I)
      I
}
##In the next lines you can see the functions working.

##> source('~/Rprogramming/ProgrammingAssignment2/cachematrix.R')
##> x<-c(1:9)
##> x[9]<-1
##> dim(x)=c(3,3)
##> y<-makeCacheMatrix(x)
##> y$get()
##[,1] [,2] [,3]
##[1,]    1    4    7
##[2,]    2    5    8
##[3,]    3    6    1
##> y$getinverse()
##NULL
##> cacheSolve(y)
##[,1]       [,2]   [,3]
##[1,] -1.7916667  1.5833333 -0.125
##[2,]  0.9166667 -0.8333333  0.250
##[3,] -0.1250000  0.2500000 -0.125
##> cacheSolve(y)
##getting cached data
##[,1]       [,2]   [,3]
##[1,] -1.7916667  1.5833333 -0.125
##[2,]  0.9166667 -0.8333333  0.250
##[3,] -0.1250000  0.2500000 -0.125
># y$getinverse()
##[,1]       [,2]   [,3]
##[1,] -1.7916667  1.5833333 -0.125
##[2,]  0.9166667 -0.8333333  0.250
##[3,] -0.1250000  0.2500000 -0.125
