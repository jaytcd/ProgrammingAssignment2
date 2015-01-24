## There are two functions; it will store a matrix and then cache its result.
## they produce the inverse of the matrix, but if the matrix has already been calculated the result 
## will have been stored in the cache and so will be cached rather than calculating it again.

## this function produces a list of set, get, setinverse, getinverse functions that cacheSolve uses 
## to produce the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set<- function(y) {
        
                x<<- y
                m<<-NULL
                
        }
        
        get<- function()x
        
        setinverse <- function (solve) m<<-solve
        getinverse<- function()m
        
        list(set= set, get=get, setinverse = setinverse, getinverse, getinverse)
                
}


## this function will check if the result has already been calculated and therefore already been cached, if so it 
## will get that result. If the result hasnt been calculated then it will produce the inverse of the matrix m

cacheSolve <- function(x, ...) {

        m <- x$getinverse
        
        if(!is.null(m))
        {
                message("getting cached data ")
                return(m)
        }

        data <- x$get()
        m<- solve(data)
        x$setinverse(m)
        m
        
        ## Return a matrix m that is the inverse of 'x'
}

## i.e. for example if we want to produce the inverse of the matrix b 
## where b is equal to  >b <- matrix(c(1,1,1,3,4,3,3,3,4), nrows = 3, ncol = 3)
## then we would proform the calculation by:
##                       >cacheSolve(makeCacheMatrix(b))
## which will produce the output
##  7  -3  -3 
## -1   1   0
## -1   0   1



