## RProg Assignment 2 : SSG
##
## These two functions allow a matrix inverse to be calculated and cached. If the matrix inverse is requested
## again after the first calculation, the cached version of the inverse is provided and the inverse is not 
## recalculated.
#############################
## Example usage : 
## > mcm <- makeCacheMatrix(matrix(1:4,2,2))
## > mcm$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > 
## > cacheSolve(mcm)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(mcm)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
#############################

## makeCacheMatrix() takes a matrix and returns a list which :
      #Sets the matrix to a new matrix
      #Gets the matrix
      #Sets the matrix inverse
	#Gets the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                              # Prepares inv to be the inverse
        set <- function(y) {                     # Sets the matrix to a new matrix and resets the inverse
                x <<- y
                inv <<- NULL
        }
        get <- function() x                      # Allows access to the matrix
        setinv <- function(solve) inv<<- solve   # Calculates the inverse and sets it inv
        getinv <- function() inv                 # Allows access to the inverse 
        list(set = set, get = get,               # Returns a list of the functions
             setinv = setinv,
             getinv = getinv)

}



## cacheSolve() looks for a cached inverse of the matrix and returns it. Otherwise, it calculates, caches and retuns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                         # Grab any cache of the inverse
        if(!is.nullmatrix(inv)) {                 # See if the cache contains the inverse. If so, return it
                message("getting cached data")    # mentioning that this is the cached version.
                return(inv)
        }
        data <- x$get()                           # No cache found. Grab the matrix.
        inv <- solve(data, ...)                   # Calculate the inverse
        x$setinv(inv)                             # Cache the inverse.
        inv                                       # Return the inverse
}

