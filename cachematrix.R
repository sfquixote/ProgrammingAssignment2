## This function creates a matrix object capable of caching its inverse
makeCacheMatrix <- function(x=matrix()){
      invrs <- NULL                 #Initialize the inverse propert
      set <- function(y){           #Set the matrix
            matrix <<- y
            invrs <<- NULL
      }
      get <- function(){            #Retrieve the matrix
            matrix
      }
      setInverse <- function(inverse) {   # Set and store the inverse of matrix
            invrs <<- inverse
      }  
      getInverse <- function() {          #Retrieve the inverse
            invrs
      }
      
      list(set = set, get = get,          #Retrieves the list of methods
           setInverse = setInverse,
           getInverse = getInverse)
}

## The next function computes the inverse of the special matrix returned by the "makeCacheMatrix" function. 
## If the inverse has been calculated, then "cachesolve" should retrieve the inverse from the cache in Part I.
## If the inverse wasn't yet been calculated, the function moves to Part II
cacheSolve <- function(x, ...) {  
      invrs <- x$getInverse()    ## Return a matrix that is the inverse of 'x'
      
      ## Part I: logical test - has inverse been previously calculated and cached? If so, it retrieves.
      if(!is.null(invrs)) {
            message("getting cached data")
            return(invrs)
      }

      ## Part II: If inverse is null, then function gets matrix, calculates inverse, and stores in cache
      data <- x$get()  #retrieve x
      m <- solve(data) %*% data   #calculate inverse
      x$setInverse(m)  #store new (inverse) matrix
      m #and return matrix m, inverse of x
}