
## makeCacheMatrix creates a list of 4 functions that are used to set and retrieve a special stored matrix (x) 
## and its inverse (invx)

makeCacheMatrix <- function(x = data.frame()) {

        set <- function(y = matrix()) {
                x <<- y
        }
        get <- function() {
                x
        }        
        setcache <- function(x) {
                invx <<- solve(x)
        }
        
        getcache <- function(x) {
                invx
        }
        
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}

## cacheSolve inverts a matrix (x) to return its inverse (invx)
## Calls the parts of a constructor function (makeCacheMatrix) stored in a list (z) to either retrieve 
## a cached solution (if the stored matrix and new matrix are identical) or request a new solution

cacheSolve <- function(x, ...) {       ## Return a matrix that is the inverse of 'x'
        oldx <- z$get()
        if (identical(x,oldx)) {        ## Test whether the cache is still valid
                message("Using cached inverse")
                invx <- z$getcache(x)
        }
        else{
                message("Nothing cached")
                z$setcache(x)
                z$set(x)                ## Save matrix to compare to next time
        }
        invx

}
