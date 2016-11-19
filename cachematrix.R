## Programming Assignment 2
## This set of functions demonstrate the principles of lexical scoping.
## makeCacheMatrix constructs a set of functions and returns them
## within a list to the parent environment.
## solveMatrix uses these functions and the likewise handled arguments to either
## retrieve data from cache or calculate anew.

## The first function makeCacheMatrix
## creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) { # initialize x as a function argument
        m1_inverse <- NULL      #m1_inverse is set to NULL, intializing it as an
                                #object within the makeCacheMatrix environment
                                #to be used by later code
        set <- function(y) {
                x <<- y         #assigns the input argument to the x object in the
                                #parent environment
                m1_inverse <<- NULL     #empties the m1_inverse object in the
                                        #parent environment with every function
                                        #call of makeCacheMatrix
        }
        get <- function() x     #defines the getter for the matrix x
        setinv <- function(solve) m1_inverse <<- solve
                                #defines the setter for the solved inverse matrix
                                #and assigns the input argument to the value of
                                #of m1_inverse in the parent environment
        getinv <- function() m1_inverse #defines the getter for the inverse
                                        #matrix m1_inverse
        list(set = set,         #forms a list with
             get = get,         #the 4 previously built functions, names them,
             setinv = setinv,   #and returns the list as
             getinv = getinv)   #an argument to the parent environment
                
}


## The second function cacheSolve
## requires an argument that is returned by makeCacheMatrix
## in order to retrieve the inverse from the cached value that is
## stored in the makeCacheMatrix object's environment

cacheSolve <- function(x, ...) {#returns a matrix that is the inverse of 'x'
        m1_inverse <- x$getinv()#tries to retrieve data for m1_inverse from
                                #cache through the makeCacheMatrix function
        if(!is.null(m1_inverse)) { #if the retrieved data is not empty...
                message("getting cached data")
                return(m1_inverse) #...it returns previously cached data and
                                   #ends processing here
        }
        data <- x$get()         #sets data equal to the submitted matrix data,
                                #AKA x, AKA m1
        m1_inverse <- solve(data, ...)
                                #by applying m1 to the solve() function the
                                #result is the inverse matrix m1_inverse
        x$setinv(m1_inverse)    #stores m1_inverse into cache to be found in
                                #the parent environment
        m1_inverse              #delivers m1_inverse as result of the function
                                #cacheSolve to the console
}
