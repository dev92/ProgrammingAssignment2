## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The makeCacheMatrix function creates a special 
# matrix object which stores the original matrix object
# and also caches the inverse of the given matrix, if we 
# need to change the matrix we can use the set() to modify
# the matrix and then it nullifies the inverse if it already 
# exists.

# matinv - stores the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  matset <- function(y = matrix()){
    x <<- y
    matinv <<- NULL
  }
  matget <- function() x
  setmatinv <- function(inverse) matinv <<- inverse
  getmatinv <- function() matinv
  list(matset = matset,matget = matget, 
       setmatinv = setmatinv, getmatinv = getmatinv)
}


## Write a short comment describing this function

# The cacheSolve() function takes the special matrix object created
# using the makeCacheMatrix() function as an input and checks
# if an inverse exists and fetches the matrix inverse if its in cache
# else it calculates the Inverse using solve() and saves it in the cache
# for the given matrix object.

# IMPORTANT NOTE : Unless you create a special matrix object 
# using makeCacheMatrix(), cacheSolve() function will fail.
# I have added a commented line explaining about creating a 
# special matrix object by giving matrix directly as an input.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # x <- makeCacheMatrix(x) #uncomment this function if you want to create
                       #special matrix object and also obtain matrix  
                       #inversion within same function.
   matinv <- x$getmatinv()
   data <- x$matget()
   if(!is.null(matinv)){
       message("getting cached matrix inverse..")
       return(matinv)
     }
   matinv <- solve(data)
   x$setmatinv(matinv)
   matinv
}
