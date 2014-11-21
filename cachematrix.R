## The initial function makeCacheMatrix creates a special "matrix" that can cache it inverse
## The second function cacheSolve computes the inverse of the special "matrix"

## The makeCacheMatrix creates a special "matrix" for inputting to cacheSolve function
## It sets the inverse of the matrix and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      Matrix <- NULL
      ## sets the original matrix
      set <- function(y){
              x <<- y
             Matrix <<- NULL
     }
     ## gets the matrix
     get <- function() x
     ## sets an inverse matrix with solve(used to calculate the inverse) and stores 
     ##it in variable Matrix
     setInMatrix <- function(solve) Matrix <<- solve
     getInMatrix <- function() Matrix
     ## lists the new set function names
     list(set = set, get = get,
          setInMatrix = setInMatrix,
          getInMatrix = getInMatrix)
}


## The function cacheSolve computes the inverse of the special "matrix"
## If the inver has been calculated, then the cachsolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
      Matrix <- x$getInMatrix()
      ## checks for an inverted matrix to return
      if(!is.null(Matrix)){
            message("getting cached data of the inverse matrix")
            return(Matrix)
      }
      ## this creates an inverted matrix if there is not a cached matrix 
      data <- x$get()
      Matrix <- solve(data, ...)
      x$setInMatrix(Matrix)
      Matrix        ## Returns a matrix that is the inverse of 'x'
}



