makeCacheMatrix <- function(x = matrix()) { ## Initialise a function where x is an empty matrix. The reason for setting an expty matrix object is so that later on when we assign a matrix with figures (for example example <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)), it will keep the figures cashed
  m <- NULL ## set the value object m to null. The reason for setting objects at this point is for future use. Later, when the cashSolve is called, it first checks whether m has a value stored in the cash (parent environment). 
  set <- function(y) { ## The set function is used to 
    x <<- y ## Assign, to the parent environment, the value of y to x. This row and the row below will essenatially erase the value stored in the cache if the matrix is a new matrix. This will also essentially make the call for cacheSolve
    m <<- NULL ## Assign, to the parent environment, the value of NULL to m
  } ## This is the end of the first part of this function which assigns the value of the matrix to the parent environmet 
  ## Now that the value of x and m are sored in the parent environment and for us to later on be able to access these values with the CacheSolve function, we will set new objects below
  get <- function() x ## This function will use the value of the vector x used above. 
  setinverse <- function(inverse) m <<- inverse ## This will set the new inverse of a new matrix in the parent environemnt 
  getinverse <- function() m ## This will allow us to get the inverse later.
  list(set = set, get = get, ## This will allow us to access the above functions (get, set, setinverse, getinverse) through a list (turning the functions into a list) by using the $ operator later
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) { ## cacheSolve is a function of x (which is stored in the parent environment) which will essentially look up the cached inverse of a matrix or calculate it if nothing exists in the parent environment or a new matrix is stored in the parent environment
  m <- x$getinverse() ## This will go and get the getinverse value stored above.
    if(!is.null(m)) { ## This will check if m (the inverse of the matrix) is NULL. If it is it will continue down the function. If not and m has a value, it will return that value back
      message("getting cached data") ## If m has a valid value, it will prompt a messege saying "getting cashed data"
      return(m) ## Return m to the console from the parent environemnt
    }
  data <- x$get() ## THis will retrive the matrix defined above and assign it to data
  m <- solve(data, ...) ## This will sove the inverse of the matrix "data"
  x$setinverse(m) ## This will retrive the 
  m ## Print the inverse of the matrix
}
