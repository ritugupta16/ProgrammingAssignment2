# "makeCacheMatrix" function - Creates a special "matrix" object and caches its inverse.
# "cacheSolve" function - Checks the cache if inverse of this special "matrix" has been computed by "makeCacheMatrix" function. 
# If already computed so, "cacheSolve" retrieves the inverse matrix from cache.Else, it will compute the inverse matrix and cache this output.


# To create a special "matrix" object and caches its inverse.
makeCacheMatrix<- function(x = matrix()) {
  i<- NULL
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getMatrix <- function() {
    x
  }
  setinverse<- function(inverse){
    i <<- inverse
  }
  getinverse <- function() {
    i
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


# To check if inverse matrix exists, if not then compute it and add it to cache.

cacheSolve<- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# Below are two test cases to test above implementation.


# TESTING-EXAMPLE 1

#Bella<-matrix(c(1, 2, 3, 4), 2,2)
#im<-makeCacheMatrix(Bella)
#cacheSolve(im) # inverse matrix returned after computing.
#cacheSolve(im)# inverse matrix returned from cache.


# TESTING-EXAMPLE 2

#r1<-c(1,2,3)
#r2<-c(0,1,5)
#r3<-c(5,6,0)
#Roma<-rbind(r1,r2,r3)
#ir<-makeCacheMatrix(Roma)
#cacheSolve(ir) # inverse matrix returned after computing.
#cacheSolve(ir)# inverse matrix returned from cache.






