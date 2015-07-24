## The two functions can be used to cache the inverse of a matrix.
## The function makeCacheMatrix creates an object that has functions to set and get a matrix and to 
## set and get its inverse. 
## The function cacheSolve checks if the inverse has already been computed. If yes it gets the 
## cached value, if not it computes the inverse and caches it.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## input: matrix x
## output: list of functions that set/get the matrix, set/get the
##         inverse
makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL

    # function to set the matrix
    set <- function(y) {       
        x <<- y
        inverse <<- NULL
    }
    # using '<-' would assign these values only inside the enviroment of the set-function.
    # '<<-' assigns the values to 'x' and 'inverse' in the parent environment, which is the 
    # environment of the makeCachMatrix function

    # function to get the matrix
    get <- function() x
    # 'x' is in the environment of the get function a free variable because R uses Lexical Scoping 
    # it searches for the value of 'x' in the parent environment, which is the environment of the 
    # makeCacheMatrix function

    # function to set the inverse
    setInverse <- function(inverseOfMatrix) {
        inverse <<- inverseOfMatrix
    }
    # using '<-' would only assign the value to the 'inverse' object inside the environment of the 
    # setInverse function the 'inverse' variable in the parent environment would not be affected.
    # '<<-' overwrites the value of 'inverse' in the environment of the makeCacheMatrix function

    # function to get the inverse
    getInverse <- function() inverse
    # 'inverse' is a free variable inside the environment of the getInverse function, so R searches 
    # for its value in the parent environment and thus returns the value of 'inverse' in the 
    # makeCacheMatrix environment

    # put all four functions into a list
    list( set = set
        , get = get
        , setInverse = setInverse
        , getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix if the inverse is cached the value is returned from 
## cache, otherwise the inveres gets computed and cached
## input: makeCacheMatrix
## output: inverse
cacheSolve <- function(x, ...){

    # try to get the cached inverse
    inverse <- x$getInverse()

    # if the inverse has already been computed just return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    # else
    # get the original matrix
    mat <- x$get()

    # compute the inverse
    inverse <- solve(mat, ...)

    # cache the inverse
    x$setInverse(inverse)

    # return the inverse
    inverse
}
