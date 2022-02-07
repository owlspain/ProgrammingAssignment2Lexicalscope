## makecachematrix is a function which serves as getter and setter
## for data and inverse matrix cacheSolve finds the inverse of a non-singular 
## matrix if its first time else caches the value.

## getter and setter function

makeCacheMatrix <- function(x = matrix()) {
  r<-nrow(x)
  c<-ncol(x)
 # To check if the matrix is a square matrix 
  if (r != c)
  {
    print ("Not a square Matrix")
    return
  }
  ## defaulting inverse matrix to be an identity matrix
  i<-diag(r)
  ## set function to set the value to parent environment
  set<-function(y){
    x<<-y
    i<<-diag(r)
  }
  ## getter function to get data
  get<-function()x
  ## setter function to set inverse
  setinverse<-function(solve)i<<-solve
  ## getter function to get inverse
  getinverse<-function()i
  #list to the getters and setters so it can be accessed.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function to use solve to find the inverse if its first time else 
## else cache from memory
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  r<-nrow(i)
  if(i!=diag(r))
  {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  if (is.singular.matrix(data)==FALSE)
  {
    i<-solve(data,...)
    x$setinverse(i)
    return(i)
  }
  else
  {
    print("Singular Matrix")
  }
}
