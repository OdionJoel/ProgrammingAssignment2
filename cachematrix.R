'''
These functions are used for caching the inverse of a matrix and efficiently computing it:
1. `makeCacheMatrix`:
   - This function creates a special matrix object that can cache its inverse.
   - It initializes an empty cache (`NULL`) to store the cached inverse.
   - It defines three functions:
     - `set`: Sets the value of the matrix and clears the cache.
     - `get`: Gets the value of the matrix.
     - `cacheInverse`: Calculates and caches the inverse of the matrix. If the inverse is already cached, it retrieves the cached data.
2. `cacheSolve`:
   - This function is used to compute the inverse of a cached matrix.
   - It checks if the `matrixCache` object has a valid `cacheInverse` function.
   - If a cached inverse exists, it retrieves it; otherwise, it computes the inverse and caches it using the `cacheInverse` function.
Overall, these functions provide a way to efficiently compute and cache the inverse of a matrix, avoiding redundant calculations when the same matrix is used multiple times.
'''

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  get <- function() x
  cacheInverse <- function() {
    if (!is.null(cache)) {
      message("Getting cached data")
      return(cache)
    }
    message("Calculating inverse and caching")
    inv <- solve(x)
    cache <<- inv
    inv
  }
  
  list(set = set, get = get, cacheInverse = cacheInverse)
}


cacheSolve <- function(matrixCache) {
  if (!is.function(matrixCache$cacheInverse)) {
    stop("matrixCache must be created with makeCacheMatrix.")
  }
  cachedInverse <- matrixCache$cacheInverse()
  if (is.null(cachedInverse)) {
    message("No cached data found, computing inverse")
    cachedInverse <- matrixCache$cacheInverse()
  }
  cachedInverse
}


mat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))
