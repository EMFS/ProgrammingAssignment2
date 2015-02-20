## Creates a type of Matrix associated with a cache to keep its inverse, in
## order to avoid needless recomputations.
## Code adapted from the functions in the assignment example "Caching the Mean
## of a Vector"


## Funtion to create the matrix with its associated cache for an inverse

makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrix_inverse <<- inverse
        getinverse <- function() matrix_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the cached matrix

cacheSolve <- function(x, ...) {
        matrix_inverse <- x$getinverse()
        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        data <- x$get()
        matrix_inverse <- solve(data, ...)
        x$setinverse(matrix_inverse)
        matrix_inverse
}

# Basic testing:
# 1. On Id matrix 2 x 2:
# test_id <- matrix(c(1,0,0,1), nrow=2)
# cache_id <- makeCacheMatrix(test_id)
# cacheSolve(cache_id)
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1

# 2. On an invertible matrix 3 x 3:
# test_3_3 <- matrix(c(1,0,5,2, 1, 6, 3, 4, 0), nrow=3)
# cache_3_3 <- makeCacheMatrix(test_3_3)
# inv_test <- cacheSolve(cache_3_3)
# inv_test %*% test_3_3 == test_3_3 %*% inv_test == Id

# 3. Should time a loop with inversions to verify the gains in performance...