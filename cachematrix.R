# The purpose of the two functions is to avoid time-consuming 
# computations by:
# (1) calculating the inverse of a matrices;  
# (2) caching these values (i.e. strong this value in the parent 
# environment);
# (3) allowing the cached value of a particular matrix to be 
# retrieved. 

## MakeCacheMatrix() is meant to be run only once for each
## particular matrix X.Because we are caching a new matrix, it first 
## resets the value the stored inverse in the global environment. 
## it then creates four functions: the set() function to change
## x and store it in the parent environment; the get() function to 
## retrieve x; the setinverse() function to change the inverse of x
## and store it in the parent environment; and the getinverse() 
## function to retrieve the inverse of x. The generation of a list 
## ensures each of the functions can be called using $.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## CasheSolve() retrieves the cached value of an inverse if 
## available and, if not, calculates the inverse.After calculating
## the inverse, it caches it by calling on the setinverse() function
## of makeCacheMatrix.As a result CasheSolve() only has to calculate
## the inverse of each matrix once. The message "Getting cached 
## data!" will be displayed if cached data is retrieved. 


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data!")
                return(i)
        } 
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}


## Return a matrix that is the inverse of 'x'
## 1 creating two test matrices. 
x <- matrix(1:4, 2, 2)
z <- matrix(4:1, 2, 2)
test_matrix <- makeCacheMatrix(x)
test_matrix2 <- makeCacheMatrix(z)
## 2. Running the CacheSolve function for the first time requires
## the inverse to be calculated.
cacheSolve(test_matrix)
cacheSolve(test_matrix2)
## 3. Running the CacheSolve function again allows the 
## retrieval of cached data for each particular matrix. 
cacheSolve(test_matrix)
cacheSolve(test_matrix2)
