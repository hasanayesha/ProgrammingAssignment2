## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#creates a matrix with the following functions
#1. set the value of the matrix. setM
#2. get the value of the matrix. getM
#3. set the value of the inverse. setI
#4. get the value of the inverse. getI
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #setting cache as null
  
  set <- function(y){
    x <<- y #assigning input y to matrix x 
    #in the parent environment
    m <<- NULL #reinitializing m to null.
  }
  
  get <- function() x #returns the matrix
  
  setInverse <- function(solve){ #sets the inverse of the matrix
    m <<- solve
  }
  
  getInverse <- function() m #returns the inverse of the matrix
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
#this functions calculates the inverse of the matrix by first checking if the inverse
#has been calculated. If yes, it returns the cache else returns the new calculated value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse() 
        
        if(!is.null(m)){ #checks if the matrix has been cached
          message("getting cached data")
          return(m)
        }
        
        temp <- x$get() #gets the input matrix
        
        m <- solve(temp, ...) #computes the inverse
        
        x$setInverse(m) #sets the inverse
        
        m #returns the inverse of the matrix
}
