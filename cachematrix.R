# Matrix inverse in case of large matrix more than one time at the same session will 
# consume cpu time and resources.
# so,This function will cache the inversed matrix after it called the first time
# it can be used afterwards repeatedly without apply the inverse function"solve()" again

 
makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y) 
        {
                x <<- y ## is equivalent to assign("name", value, inherits = TRUE)
                        ## parent environments
                 inv <<- NULL
        }
       
        get <- function() x
       
        setinverse <- function(inverse) inv <<- inverse
       
        getinverse <- function() inv
      
      #create a list of all variables  
        list(set=set,##
             get=get,## 
             setinverse=setinverse,##
             getinverse=getinverse)##
}
############################################
#cacheSolve fuction: do the following:
# try to get the inversed matrix if exist from x environment
# checks if inverted matrix exsits(cached before)
# If it exists,do nothing ,just print the inverted matrix and print a message.
# if not exist, get the original matrix and invert it.Store the inversed matrix 
# in the x environment.

# This function caches the inversed matrix for the first time,it contains other functions
##That are called from other function.
cacheSolve <- function(x, ...) 
{
        inv <- x$getinverse() 
       
        if(!is.null(inv))
        {##Check if have value computed from before
                message("getting cached data.")
                return(inv)##Exit,Here,donot complete
        }##The following lines will be called only one time
       
        data <- x$get()##calling from another environment x$
        
        inv <- solve(data)## solve: inverse the matrix using
        
        x$setinverse(inv) ## set the inversed value " x$inv" in environment denoted by x
        
        inv
}

## Sample run:
x = rbind(c(1, 5), c(-5, 1))
cachedInverse = makeCacheMatrix(x)
cachedInverse$get()
 

## Note: first time, not cached yet
cacheSolve(cachedInverse)
 
## lets run  again
## You see a message before the return indicating the value will be the cached version
 
cacheSolve(cachedInverse)
 
