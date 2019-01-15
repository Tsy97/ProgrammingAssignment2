## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeMatrix<- function(x = matrix()){
        mymatrix<- NULL
        
        ##set the value of the matrix
        set<- function(y){
                x<<- y
                mymatrix<- NULL
        }
        
        ##get the value of the matrix
        get<- function() x
        
        ##set the value of the inversed matrix
        setmatrix<- function(inversed.matrix) mymatrix<<- inversed.matrix
        
        ##get the value of the inversed matrix
        getmatrix<- function() mymatrix
        
        ##return a list of all values
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

cacheSolve <- function(x, ...) {
        ##get the value returned by getmatrix function
        mymatrix <- x$getmatrix()
        
        ##test if mymatrix has already been inversed, if true, just return the value
        if(!is.null(mymatrix)) {
                message("getting cached data")
                return(mymatrix)
        }
        
        ##if the value of mymatrix has not been inversed
        ##get the value of the original matrix returned by get function
        data <- x$get()
        
        ##cache the value of inversed matrix 
        inversed.matrix <- solve(data, ...)
        
        ##set the value of inversed matrix to mymatrix variable
        x$setmatrix(inversed.matrix)
        
        ##returen the value of inversed matrix
        mymatrix
}
