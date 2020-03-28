##The functions creates a square matrix and then inverts the square matrix


##makeCacheMatrix first initializes x as a function and sets the default of x
##to an empty 1 by 1 matrix
##it then assigns to the variable cache a default value of NULL
##The set function sets the value of y to be the value of x in the parent enviornment (in this case a matrix)
##The set function allows the matrix and cache to be reset without having to call the entire function
##The list allows all getter and setter arguments to be easily called using $ operator

makeCacheMatrix<-function(x=matrix()){
  cache<-NULL
  set<-function(y){
    x<<-y
    cache<<-NULL
  }
  get<-function()x
  setinverse<-function(solve)cache <<- solve
  getinverse<-function()cache
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Without cacheSolve, makeCacheMatrix is incomplete
## cacheSoleve takes the output of the makeCacheMatrix function and calculates the inverse
## First it checks to see whether cache is still NULL or has already been assigned a value
## It it is not the formula returns a message and the value of cache already calculated 
## If it the value of cache is NULL, the function continues
## The second part of the function gets the martrix made by makeCacheMatrix and assigns it the name data
## it then uses the solve function to calculate the inverse of data and assigns it the name cache
## The remainder of the function sets the inverse to cache and prints the inverted matrix


cacheSolve<-function(x,...){
  chache<-x$getinverse()
  if(!is.null(cache)){
    message("getting chached data")
    return(cache)
  }
  data<-x$get()
  cache<-solve(data,...)
  x$setinverse(cache)
  cache
}
