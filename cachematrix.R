## The function caches the inverse of the matrix. 
##It inserts the inverse of the matrix in some special cache. 
##If the matrix already had its inverse in the cache, function gets the inverse from the cache, instead of calculating. 


## function prepares the special type of matrix

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) { #setting the value of the input matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x # getting the value of the matrix
        setSolve <- function(solve) m <<- solve # setting the value of the inverse of the matrix
        getSolve <- function() m # getting the value of the inverse matrix
        list(set = set, get = get, # saving the cached matrix and its inverse in the special matrix type
             setSolve = setSolve,
             getSolve = getSolve)
}


## function calculated the inverse of the matrix

cacheSolve <- function(x, ...) { # function calculated the inverse of the matrix
        m <- x$getSolve() # inserts into m the value of getSolve from the special matrix cache
        if(!is.null(m)) { # checks if the matrix exists in the special matrix prepared in makeCacheMatrix
                message("getting cached data")
                return(m)
        }
        data <- x$get() # getting the inverse from the cache
        m <- solve(data, ...) # setting the inverse of the matrix
        x$setSolve(m) # setting the inverse of the matrix from the cache
        m
}
