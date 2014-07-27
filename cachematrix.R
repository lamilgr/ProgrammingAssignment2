## Function makeCacheMatrix () creates a list of matrix functions 
## that facilitates setting and getting values of cached matrix objects

makeCacheMatrix <- function(x = matrix()) {
        s<- NULL
        set <- function(y){
                x<<- y
                s<<- NULL
        }
        get <- function() x
        setsolve <- function (solve) s<<-solve
        getsolve <- function () s
        list(set= set, get= get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Fucntion cacheSolve() calculates the inverse of a cached matrix object 
## as long as the calculation has not been cached already. In that case 
## it returns the cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if (!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s<- solve(data)
        x$setsolve(s)
        s
}
