## Cache the inverse of a matrix to save computational power when needing
## to call the inverse that has already been computed

## create a function to store the inverse of a matrix in cache
makeCacheMatrix <- function(x = matrix()) {
	inv_mtrx <- NULL
	##set the value of a special matrix in another enviroment
	set <- function (y) {
		x <<- y
		inv_mtrx <<- NULL
	}
    ## get the value of the matrix
	get <- function() x
	## set the value of the inverse of the matrix
	SetInv <- function(invert) inv_mtrx <<- invert
	## get the value of the inverse of the matrix
	GetInv <- function() inv_mtrx
	list(set = set, get = get,
		SetInv = SetInv,
		GetInv = GetInv)	
}


## Solve for the inverse of a matrix, unless it has already been solved
## and stored in cache, in which case, get it from cache

## create a function to get the inverse of a matrix from cache if it exists.
cacheSolve <- function(x, ...) {
    inv_mtrx <- x$GetInv()
    ## check for existing value of inverse of the matrix and return it.
    if (!is.null(inv_mtrx)) {
        message("getting cached data")
        return(inv_mtrx)
    }
    ## compute the inverse of the matrix if it doesn't exist in cache
    ## and return it.
    mtrx <- x$get()
    inv_mtrx <- solve(mtrx, ...)
    x$SetInv(inv_mtrx)
    inv_mtrx
}