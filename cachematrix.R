makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y       ###The use of scoping
    m <<- NULL    ###The use of scoping
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  m <- x$getInv()  ###Get possibly saved matrix inverse
  if(!is.null(m)) {
    message("getting cached inverse of the matrix")
    return(m)
  }
  data <- x$get() ###In case cached matrix inverse cannot be found, copute it
  m <- solve(data, ...)
  x$setInv(m)
  m
}

### Sample use:
###           u<-matrix(c(1,2,1,3),nrow=2,ncol=2)
###           v<-makeCacheMatrix(u)
###           w<-cacheSolve(v)
              w                 ####Show the w matrix
###           w%*%u             ####validate, you should get a unit matrix here
###           w<-cacheSolve(v)  ####You should get the "getting cached inverse of the matrix" and the matrix inverse as well
