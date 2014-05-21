# This function is an object that encapsulates the data and the 
# methods that act on the data.  It returns a list that allows
# the caller to mutate and access the internal fields.

makeCacheMatrix <- function(x = matrix()) {

   inv <- NULL

   set <- function(m)  {
      x   <<- m
      inv <<-  NULL
   }

   get <- function() x

   setinv <- function(inverse) inv <<- inverse
   getinv <- function() inv

   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function is the main driver of the program.  Use as:
#
# > mat = matrix(c(1,2,3,4), nrow=2, ncol=2)
# > f <- makeCacheMatrix(mat)
# > cacheSolve(f)

cacheSolve <- function(f, ...) {

   inv <- f$getinv()
   if (!is.null(inv))  {
      message("getting cached matrix inverse")
      return (inv)
   }

   data <- f$get()
   # Do some sanity checks.
   if (dim(data)[1] != dim(data)[2])  {
      message("matrix is not square")
      return(0)
   }
   if (det(data) == 0)  {
      message("matrix is not invertible")
      return(0)
   }
   # End of checks
   inv  <- solve(data, ...)
   f$setinv(inv)
   inv
}

# mat1 = matrix(c(1,2,3,4), nrow=2, ncol=2)
# mat2 = matrix(c(1,2,4,4,5,4,7,8,9), nrow=3, ncol=3)
