## efficient matrix inverse computation 
## The functions below create a special matrix for which inverse would be computed only once
## This matrix is created using "makeCacheMatrix"
## the inverse is computed and cached or returned from cache using "cacheSolve"

## makeCacheMatrix creates a list containing four functions , get, set, setInverse, getInverse
## get and set manipulate the value of a matrix
## getInverse and setInverse manipulate the inverse of the said matrix

makeCacheMatrix <- function(x = matrix()) {
	    ## initially inv is NULL
	    inv <- NULL
	
	    ## create functions to set the value of the matrix
	    set <-function(y) {
		    x <<-y
		    m <<-NULL
	    }

	    ## get the value of the matrix
            get <- function() x
	    ## set the inverse for this matrix
	    setInverse <-function (mi) inv <<- mi
	    ## get the cached inverse for this matrix
	    getInverse<- function() inv
	    
	    ## actually create the list and return it
	    list (set = set , get = get, setInverse = setInverse , getInverse = getInverse)
}


## cacheSolve function works on the object created by makeCacheMatrix to compute the inverse of the matrix
## the inverse is cached , in case the inverse does not exist , NULL value is returned.

cacheSolve <- function(x, ...) {
        
        ## check if the inverse already exists 
        inv<-x$getInverse()
        # if the value is not null return 
        if(!is.null(inv)) {
        	message("getting cached data")
        	return (inv)
        }

        ##get the matrix to invert
        data<-x$get()

        ## actually attempt to compute the inverse
        inv<-try(solve(data),TRUE)
        ##if the reutrned the value is not a valid matrix then the inversion failed
        if(!is.matrix(inv)) {
        	message("error: no inverse")
        	inv=NULL
        }

        ## set the inverse to the cached value
        x$setInverse(inv)
        ##return the inverse
        return (inv)
}



