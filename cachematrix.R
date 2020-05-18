# These two functions work together to store the value of a square matrix and its inverse and 
# then call the inverse value when needed.

# This function returns a list of functions which sets the value of the square matrix and its inverse in a cache.

makeCacheMatrix <- function(x = matrix()) {
        inn <- NULL
        set<- function(y){
                x<<-y
                inn<<-NULL
        }
        get<- function()x
        setinn<- function(solve) inn<<- solve
        getinn<- function()inn
        list(set=set, get=get,setinn=setinn,
             getinn=getinn)
}


# This function takes the functions and values from the makeCacheMatrix function
# and returns the inverse of that matrix (if possible).
# If the value is not stored then it will calculate the inverse.

cacheSolve <- function(x, ...) {
        inn<-x$getinn()
        if(!is.null(inn)){
                message("retrieving from cache")
                return(inn)
        } 
        data<-x$get()
        inn<-solve(data, ...)
        x$setinn(inn)
        inn
}
