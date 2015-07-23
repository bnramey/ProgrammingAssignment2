## This is two functions 
## First one creates a special matrix object that can cache it's inverse
## Secondcomputes the inverse of this special matrix
## makeCacheMatrix returns a list of functions that 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setinv<-function(inverse) m<<-inverse
	getinv<-function()m
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve will compute the inverse of the matrix
## if the inverse was previously calculated, it returns the cached inverse

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	## If the inverse is already calculated then get cached value
	invert<-x$getinv()
	if(!is.null(invert)){
		message("Getting Cached data")
		return(invert)
	}
## if the inverse isn't already calculated then calculate the inverse
	data<-x$get()
	invert<-solve(data,...)
	x$setinv(invert)
## invert is the inverse of the matrix x
	return(invert)
}
## returns a matrix that is the inverse of "x"

