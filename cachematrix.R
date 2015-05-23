##  This script provides a user inverse of an invertible matrix. If the result is already calculated then
##  it is provided thorugh cache, otherwise it is calculated and cached for future enquiries.


##  makeCacheMatrix function gets a matrix as input parameter and checks it for square matrix,computes and
##  caches the inverse of the input matrix. This function returns a list of functions for
##    1. getting the value of input matrix
##    2. getting the cached value of inverse.
##    3. setting the inverse of matrix to cache.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  #check for a square matrix by comparing number of rows and cols.
  if(nrow(x) != ncol(x))
    {
      message("Please choose a square matrix to inverse.")
      return()
    }
  #This function retrievs the input matrix
  getData <- function()
    {
      x
    }
  #This function retrieves the cached inverse of matrix.
  getInverse <- function()
    {
      inv
    }
  #This function sets the inverse of matrix to inv variable.
  setInverse <-function(inverse)
    {
      inv <<- inverse
    }
  #Returns the above mentioned function in list from 
  list(getData = getData, getInverse = getInverse, setInverse = setInverse)
}


##  cacheSolve function calcualtes the inverse of matrix created with above function. Although, it first checks
##  whether the inverse is already cached. If it gets the value from cache, then it returns from there.
##  Otherwise, it first calculate and stores inverse into cache using 'setInverse' and then returns it for user.


cacheSolve <- function(x, ...) {
  
  i<- x$getInverse()    #getting value from cache
    if(is.null(i))      #checking for cached inverse
      {
        data<-x$getData() #getting input matrix
        i<-solve(data)    #calculating inverse
        x$setInverse(i)   #caching reverse
      }
    else
      {
        message("getting cached data")
      }
  i      #returning reverse of matrix.
}
