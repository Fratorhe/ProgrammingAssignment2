## These functions calculate the inverse of a squared matrix considering that the inverse exists (i.e. the matrix is not SINGULAR).
## Since calculate a inverse matrix is a computational demanding work, these functions save the result
## on cache, to be used again without calculating.
## Author: Francisco Torres. Last Modification: 25-07-2014
## Functions based on the work done by Dr. Peng in order to calculate the mean and/or obtain it from cache.
## 
## This first function generates a list in which the inverse matrix is stored when it is generated.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # Initialize the inverse matrix
        set <- function(y) { # Funtion to set the values
                x <<- y
                inverse <<- NULL
        }
        get <- function() x #gets the value of the vector from cache
        setinverse <- function(solve) inverse <<- solve #Stores the inverse
        getinverse <- function() inverse #gets the inverse from cache
        list(set = set, get = get, #generate the return vector
             setinverse = setinverse,
             getinverse = getinverse)
}


## This functon checks if the inverse is stored on the cache, if it is, it skips the calculation. If it is not, it calculates it
## and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse() #sets the value of the inverse from the previous function
        if(!is.null(inverse)) { #checks if the inverse exists. If it does, its value is returned.
                message("getting cached data")
                return(inverse)
        }
        data <- x$get() #sets data to the value of the input matrix
        inverse <- solve(data, ...) #calculates the inverse
        x$setinverse(inverse) #sets the value of the inverse on cache
        inverse #returns the inverse
}
