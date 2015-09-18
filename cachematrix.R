## These functions allow for efficient computation of a matrix's inverse by 
#  caching it, so it does not have to be recomputed later.  It takes advantage
#  of R's lexical scoping, and the <<- operator allows scoping into the
#  parent environment.


# makeCacheMatrix - creates a special "matrix" object that can cache its inverse;
#                   the four sub-functions are stored in a list.
#                   Input: an n x n invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL # initialize inv, the value of inverse inv
        
        set <- function(y) {
                # this function allows the matrix to be changed
                # the <<- assignment operator is used for scoping in parent env.
                x <<- y
                inv <<- NULL # so a new inv can be computed
        }
        
        get <- function() x # Returns matrix x in main function
        setInv <- function(inverse) inv <<- inverse # store input
        getInv <- function() inv                    # return input
        
        # store all four functions in a list
        list( set = set, get = get,
              setInv = setInv,
              getInv = getInv)
        
}


# cacheSolve - calculates the inverse of the cached matrix returned by the
#              makeCacheMatrix function above. If the inverse has already been 
#              calculated and the matrix has not changed, then it just retrieves 
#              the stored value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInv()  # get inverse from input x
        
        # verify inv exists and is not NULL
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        dataMatrix <- x$get()
        inv <- solve(dataMatrix, ...) # Use R's 'solve' function to compute inverse
        x$setInv(inv)                 # set inverse
        inv                           # return inverse
        
}
