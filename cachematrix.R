##1. Function creating a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  #Inverse property
  m <- NULL
  
  #Set the matrix value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Get the matrix value
  get <- function() x
  
  #Set the inverse of the matrix
  setInv <- function(inverse) {
    m <<- inverse
  }
  
  #Get the inverse of the matrix
  getInv <- function() {
    m
  }
  
  #Return a list 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


##2. Function computing the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  
  #Matrix inverse to x
  m <- x$getInv()
  
  #Return the inverse if its already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Get the matrix
  data <- x$get()
  
  #Calculate the inverse
  m <- solve(data, ...)
  
  #Set the inverse
  x$setInv(m)
  
  #Return the result matrix
  m
}

A<-matrix(c(1,2,3,4,7,8,5,6,7),3,3)
A1<-makeCacheMatrix(A)
cacheSolve(A1)
