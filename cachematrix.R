## These set of functions receive a matrix as input and computes its inverse
## matrix, caching the result in a internal variable for future use. The
## procedure follows the scheme laid by the README.md file, where the first
## function produces a list from a given input matrix and the second function
## uses that list to effectively compute the inverse (after checking if the
## result is no in the already in the cache). Kind of a makeshift encapsulation.
## On production code, I would not do this, preferring to insert memoisation
## into the solve() function.
## Creates a list that encapsulates the data from a matrix, functions to
## read and store (getters and setters) its inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
# I was a bit afraid of naming it 'i' and having a namespace clash with
# something. So i_m it is.
i_m <- NULL
set <- function(y) {
x <<- y
i_m <<- NULL
}
get <- function() x
setInverse <- function(inv_m) i_m <<- inv_m
getInverse <- function() i_m
list(set = set, get = get,
setInverse = setInverse,
getInverse = getInverse)
}
## Computes the inverse of a matrix using a list produced by makeCacheMatrix,
## checking the cache first. There is no error checking for when the determinant
## is null.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
i_m <- x$getInverse()
print(i_m)
if(!is.null(i_m)) {
message("...getting cached data...")
return(i_m)
}
data <- x$get()
i_m <- solve(data, ...)
x$setInverse(i_m)
i_m
}
## A test case.
## Not run:
simpleMatrix<-matrix(1:9,ncol=3)
## It is singular. We do not want that.
simpleMatrix[2,2]<--1
simpleMatrixObject<-makeCacheMatrix(simpleMatrix)
simpleMatrixInverse<-cacheSolve(simpleMatrixObject)
simpleMatrixInverse%*%simpleMatrix
simpleMatrix%*%simpleMatrixInverse
# The results are close enough to identity.
det(simpleMatrix%*%simpleMatrixInverse)
# Yup, close enough.
