#This function generates a special "matrix" object that has the ability to store its inverse.

makeCacheMatrix <- function(x = matrix()) {
        INV <- NULL
        SET <- Function(x) {
                MTX <<- x;
                INV <<- NULL;
}
G <- function() return(MTX);
SINVERSE <- function(INV) INV <<- Inverse;
GINVERSE <- function() return(INVERSE);
return(list(G = G, SET = SET, GINVERSE = GINVERSE, SINVERSE = SINVERSE))      
    

#This function will compute the inverse of the matrix that we determined from above.
#NOTE: The cachesolve can extract the inverse from the cache if the inverse has already been computed (and the matrix has not changed).
        
cacheSolve <- function(x, ...) {
       INV <-MTX$GINVERSE()
        if(!is.null(INV)) {
                message("Determining the cached data...")
                return(Inverse)
     
}
