## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeCacheMatrix<- function(x=matrix()){
        ##x is the matrix that we want calculate
        m<-NULL        ##initial value m is NULL, we have not calculated inverse yet
        set<-function(y){
                x<<-y
                m<<-NULL    ##initial value matrix x set to y
        }
        get<-function() x     ## this function "get" value of matrix
        setinverse<-function(z) m<<-z     #set value inverse in m (now m is not NULL)
        getinverse<-function() m        ##get matrix inverse value
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve<-function (x,...){
        m<-x$getinverse()   ##m receive value inverse
        if(!is.null(m)){
                ##if m is not null return m and message, end function
                message("getting checked data")
                return (m)
        }
        data<-x$get()   ##if m is NULL we get the matrix "x"
        m<-solve(data)  ##we calculate value inverse
        x$setinverse(m) ##we set m to value no NULL
        m
}