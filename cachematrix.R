## The function takes  matrix as an input and returns list of functions which allow 
## to cache any object associated with the original matrix. The functions are:
## - set(x=matrix()): Saves X as a matrix for which some calculated value need to be cached
## - get(): returns the original matrix
## - setCachedValue(valueToCache): Takes an object associated with an original matrix and caches it
## - getCachedValue(): Returns the cached value associated with an original matrix
## 

makeCacheMatrix <- function(x=matrix()) {
  
  cachedValue <- NULL
  
  set <- function(newMatrix) {
    x <<- newMatrix
    cachedValue <<- NULL
  }
  
  get <- function() x
  
  setCachedValue <- function(valueToCache) cachedValue <<- valueToCache
  
  getCachedValue <- function() cachedValue
  
  invisible(list(set = set, 
                 get = get,
                 setCachedValue = setCachedValue,
                 getCachedValue = getCachedValue))
}


## The function takes 'cacheable' matrix obtained through the function makeCacheMatrix as an input 
## and returns it inversed. 
## The inversed matrix is cached so that all subsequent calls to this function will not involve actual calculations 
## but simply take the value from the cache.

cacheSolve <- function(x, ...) {
  inversed <- x$getCachedValue()

  if(!is.null(inversed)) {
    return(inversed)
  }
  
  data <- x$get()
  inversed <- solve(data, ...)
  x$setCachedValue(inversed)
  
  inversed
}
