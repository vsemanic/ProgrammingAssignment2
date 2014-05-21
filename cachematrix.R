## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(f, ...) {

   inv <- f$getinv()
   if (!is.null(inv))  {
      message("getting cached matrix inverse")
      return (inv)
   }

   # data <- f$get()
   inv  <- solve(f$get(), ...)
   f$setinv(inv)
   inv
}

# mat1 = matrix(c(1,2,3,4), nrow=2, ncol=2)
# mat2 = matrix(c(1,2,4,4,5,4,7,8,9), nrow=3, ncol=3)
