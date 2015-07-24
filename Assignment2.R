

# Makes the matrix based on inputs and inverts it- output is the inverted version

makeCacheMatrix <- function(r,cl,...){
  x<<-matrix(c(...), nrow=r, ncol=cl) #turns the inputs into a matrix, allowing all values desired
  
  invertme <- function(x){
    inverted <<- solve(x) # inverts the matrix and makes the assignment to the global environ using superassignment operator
  }
  
  invertme(x) #returns the inverted matrix
  inverted
}

#Checks to see if the matrix inversion exists- if so, returns it, if not, recalculates and returns that

cacheSolve <- function(r,cl,...) {
  b<<-c(...) #assign the inputs to a list for later use
  j<<-matrix(b, nrow=r, ncol=cl) #turns the inputs into a matrix, allowing all values desired
  
  if (!exists('x')) { #checks to see if x is already present (the input to makeCacheMatrix)
    message1<-("First time? Hang on and I'll run it!") 
    makeCacheMatrix(r,cl,b) # if not, runs makeCacheMatrix using inputs from cacheSolve
    return(list(message1, inverted))
  }
  else if (!identical(j,x)) { #if x is there, check to see if it matches the input to cacheSolve
    message2<-("New one- hang on and I'll run it!")
    makeCacheMatrix(r,cl,b) # if not, runs makeCacheMatrix using inputs from cacheSolve
    return(list(message2, inverted)) #if it does not match, run makeCacheMatrix and return
  }
  else {
    message3<-("Already got it, retrieving!") #if x does exist and is no different from j, then just return cached value
    return(list(message3, inverted))
  }
}

cacheSolve(5,5,8,5,5,6,9,8,3,7,1,8,5,5,6,9,8,3,7,1,6,7,2,3,9,45,3) #sample matrix data to test

#input order: row, column, all the values you want
