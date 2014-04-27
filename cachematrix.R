## fuction"makeCacheMatrix" will input a matrix and output its inverse of it and stores it in  computer's memory

makeCacheMatrix <- function(x = matrix()) {
  inv-mat<-NULL  ## initialising the inv-mat
  ## set a matrix from global enviroment to 'x'
  set<-function(y){
    x<<- y
    inv-mat<<-NULL
  }
  
  get<- function() x ## display x
  setinv<-function(solve) inv-mat<<-solve ##calculates the inverse of the matrix
  getinv<-function() inv-mat ##display the calculated result
  list=(set=set,get=get,setinv=setinv,getinv=getinv)
}


## function"cacheSolve" will check the inverse of matrix whether calculated or not
## if not, calculate it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv-mat<-x$getinv()
  if (!is.null(inv-mat)){ ## check the inverse 
   message("get the cache") ## display this message
   return(inv-mat) ## and return inverse  
  }
  ##if inverse not exist, calculate it
  data<-x$get() ## put matix into 'data'
  inv-mat<-solve(data,...) ## calculate inverse
  x$setinv(inv-mat)
  inv-mat ##display it
}

