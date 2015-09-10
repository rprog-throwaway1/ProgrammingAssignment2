## A set of functions for caching the potentially
## time-consuming calculation of matrix inverses.

## Creates a wrapper that contains 
## - A matrix
## - The cached inverse for that matrix.
## with getters and setters for both.
## Argument (optional) an initial value for the matrix.
makeCacheMatrix <- function(matrix = matrix()) {
        cachedInverse <- NULL
        list(
                get = function() {matrix},
                set = function(new_matrix) {
                        matrix <<- new_matrix
                        cachedInverse <<- NULL
                },
                getCachedInverse = function(){ cachedInverse },
                setCachedInverse = function(newCachedInverse){
                        cachedInverse <<- newCachedInverse
                }
        )
}

## Argument: cacheMatrix, a list created by makeCacheMatrix
## Result: The inverse of the matrix wrapped by cacheMatrix
## Side-effect: If a cached result exists, this 
##   is returned without recalculation; otherwise
##   the result is calculated and cached for future use.
cacheSolve <- function(cacheMatrix) {
        cachedResult <- cacheMatrix$getCachedInverse()
        if(!is.null(cachedResult)){
                message("Cache hit")
                cachedResult
        }else{
                message("Cache miss. Calculating and caching...")
                matrix <- cacheMatrix$get()
                inverse <- solve(matrix)
                cacheMatrix$setCachedInverse(inverse)
                inverse
        } 
}
