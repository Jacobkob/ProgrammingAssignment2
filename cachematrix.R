#This function creates an object which is a matrix that will determine or cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        INVERSE <- NULL
        SET <- Function(x) {
                MTX <<- x;
                INVERSE <<- NULL;
}
GET <- function() return(MTX);
SETINVERSE <- function(INVERSE) INVERSE <<- Inverse;
GETINVERSE <- function() return(INVERSE);
return(list(GET = GET, SET = SET, GETINVERSE = GETINVERSE, SETINVERSE = SETINVERSE))      
    

#This function will compute the inverse of the matrix that we determined from above.
#NOTE: When the inverse is already calculated(and the matrix has not changed), this function should get the inverse from the cache.
        
cacheSolve <- function(x, ...) {
       INVERSE <-MTX$GETINVERSE()
        if(!is.null(INVERSE)) {
                message("Determining the cached data...")
                return(Inverse)
                
       }
       mat <- x$GET()
       INVERSE <- solve(MAT, ...)
       x$SETINVERSE(Inverse)
       Inverse
}
