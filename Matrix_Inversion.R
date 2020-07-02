
#This funciton takes in one argument x which is by default set to an empty matrix
#The function then proceeds to create four new function set,get,setInverse,and getInverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    #sets the values of x and inv
    set <- function(y){
        
        #Caches the values of x and inv back to RAM
        x <<- y
        inv <<- NULL
    }
    
    #gets the value of x
    get <- function() x
    
    #sets the inverse of matrix x
    setInverse <- function(inverse) inv <<- inverse
    
    #gets the inverse of matrix x
    getInverse <- function() inv 
    
    #declares the four funcitons
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

#This function then uses the functions created in makeCacheMatrix in order to determine whether or not the 
#Inputted matrix has been previously solved and stored int he Caches
cacheSolve <- function(x, ...) {
    #sets variable inv to the cached inverse variable
    inv <- x$getInverse()
    
    #Checks wheather or not the current matrix has ever been run previously and been cached
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    #In the event the object has not been cached the function then solves the matrix using the solve() function
    mat <- x$get()
    inv <- solve(mat,...)
    x$setInverse(inv)
    inv
}