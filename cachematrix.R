## The two functions are modified versions of cachemean and makevector with the following changes/additions:
##  1. Input (x) is now a matrix instead of a vectore
##  2. In makeCacheMatrix, two additional functions were added.  setlastM and getlastM.
##     setlastM is used to cache or remember the previous input matrix for which an inverse matrix was freshly calculated
##     getlastM is recalled by cacheSolve to test to see if the new input matrix and the previous versions are identical.
## 
##    Variable names:   xxxIM refers to the Inverse Matrix
##                      xxxM refers to the input Matrix


cacheSolve <-function(x, ...) {
         IM <- x$getIM()
	   if(!is.null(IM)){					# check to see if anything is in inverse Matrix from before.  If not empty then ... 
	   SeenB4<-identical(x$getlastM(),x$get())      # check to see if the new matrix is the same as the last one.
	   print(SeenB4) 						# this print out is to show that the function went through this branch.
         if(SeenB4) {
                 message("This was calculated before... returning cached Inverse...")
                 return(IM)
         }}
         data <- x$get()
         IM <- solve(data, ...)			## Changed mean(data,...) to solve(data,...)
         x$setIM(IM)
	   x$setlast(data)				## Remember the matrix just calculated.
         IM
 }

makeCacheMatrix <- function(x = matrix()) {
         IM <- NULL
         set <- function(y) {
                 x <<- y
                 m <<- NULL
         }
         get <- function() x
         setIM <- function(mean) IM <<- mean
         getIM <- function() IM
	   setlastM <- function(lm) M <<-lm		## This function is called from cachemean2 when a fresh inverse matrix is calculated and save the input matrix for the next round.
	   getlastM <- function() M               ## This function recalls the previous input matrix.
         list(set = set, get = get,
              setIM = setIM,
              getIM = getIM,setlastM=setlastM, getlastM=getlastM)
}

