#makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {     ## For data(x) specify is matrix
        m <- NULL                               ## Reset matrix
        set <- function(y) {                    ## Set values for matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                             ## Get values for matrix
        setMatrix <- function(solve) m <<- matrix       ## Set matrix for solve(inverse)
        getMatrix <- function() m                       ## Get matrix for solve(inverse)
        list(set = set, get = get,                      ## Create list for atomic type
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}

#cacheSolve

cacheSolve <- function(x, ...) {
        m <- x$getMatrix()
        if(!is.null(m)) {                       ## If not empty, retrieve from cache
                message("getting cached data")  ## This seems straight forward
                return(m)                       ## Print cached inverse matrix
        }
        mtrx <- x$get()         ## Change x$get to represent retrieving of Matrix
        m <- solve(mtrx)        ## Solve(a, b, ...) when b is missing, solve returns inverse
        x$setMatrix(m)          ## Set matrix for solve and cache
        m                       ## Print inversed matrix
}

## mat <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
## matObj <- cacheSolve(mat)
##cacheSolve(matObj) *real-time output*
##      [,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
