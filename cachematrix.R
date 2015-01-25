## Function makeCacheMatrix contains 4 function, set, for setting data matrix,
## get for getting matrix, and setInvMat/getInvMat for storing/getting inverse
## matrix.

makeCacheMatrix <- function(x = matrix()) {
        InvMat <- NULL
        set <- function (y) {
                x <<- y
                InvMat <<- NULL
        }
        get <- function () x
        setInvMat <- function(solve) InvMat <<- solve
        getInvMat <- function() InvMat
        list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
        

}


## Function cacheSolve check whether there is invertible matrix stored already 
## using getInvMat() function, if so, it displays content using getInvMat() 
## function. Otherwise it computes inverse matrix with solve() function.

cacheSolve <- function(x = matrix(), ...) {
        InvMat <- x$getInvMat()
        if(!is.null(InvMat)) {
                message("getting cached data")
                return(InvMat)
        } 
        data <- x$get()
        InvMat <- solve(data, ...)
        x$setInvMat(InvMat)
        InvMat
}
