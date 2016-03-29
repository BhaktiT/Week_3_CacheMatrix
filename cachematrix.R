############################################################################################################
## HISTORY                                                                                                 #
## Assignment Name       :- Caching the Inverse of a Matrix                                                #
## Details of Assignment :- Matrix inversion is usually a costly computation and there may be some         #
##                          benefit to caching the inverse of a matrix rather than compute it repeatedly.  #
##                          Below are a pair of functions that are used to create a special object that    #
##                          stores a matrix and caches its inverse.                                        #
## Submitted By           :- Bhakti Thatte date :- 28/03/2016                                              #
##                                                                                                         #
############################################################################################################


############################################################################################################
#                                                                                                          #
# Function Name    :- makeCacheMatrix                                                                      #
# Parameters I/P   :- Matrix                                                                               #
# Function Details :- This function creates a special "matrix" object that can cache its inverse.          #
#                                                                                                          #
############################################################################################################

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invMat <<- inverse
  getInverse <- function() invMat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


############################################################################################################
#                                                                                                          #
# Function Name      :- cacheSolve                                                                         #
# Parameters I/P     :- X, ...                                                                             #
# Parameters Return  :- Inverse of Matrix                                                                  #
# Function Details   :- This function computes the inverse of the special "matrix" created by              #
#                     makeCacheMatrix above. If the inverse has already been calculated (and the           #
#                     matrix has not changed), then it should retrieve the inverse from the cache.         #
#                                                                                                          #
############################################################################################################


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInverse()
  if (!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  mat <- x$get()
  invMat <- solve(mat, ...)
  x$setInverse(invMat)
  invMat
}




