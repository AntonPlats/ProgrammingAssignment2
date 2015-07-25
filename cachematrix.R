## R programming 
## Assignment #2
## MakeCacheMatrix 

## make a function to make a matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # inv for inverse 
   set <- function(y) { #set the inversed function of x
       x <<- y
        inv <<- NULL
    }
   get <- function() x # get the x
   setinv <- function(inverse) inv <<- inverse #set the inversed matrix of x
   getinv <- function() inv #get the inversed function of x
   list(set = set, get = get, #return a list 
        setinv = setinv,
        getinv = getinv)

}
## inverse invertable matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv() #take the value of getinv from makeCacheMatrix
    if(!is.null(inv)) {# if the inv veriable is not NULL
        message("getting cached data") #get the cached data and display a mssage
        return(inv) #exit if inv has the inverse of x
    }
    data <- x$get()# if inv is NULL get the matrix x
    inv <- solve(data, ...) #calculate the inverse of data
    x$setinv(inv) #cache the inverse of x in inv
    inv # return inv
}
