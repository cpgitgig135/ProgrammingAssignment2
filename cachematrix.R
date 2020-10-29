# Programming Assignment #2 by CP

# Code to cache the inverse of a squared matrix
# Please copy the entire contents and paste them into R to run and see how it works
# Please press the "Enter" key additional times to see the additional results of the tests at the end

# makeCacheMatrix creates a special "matrix" object that
# can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
      
        m = NULL
        set = function(y) {
                x <<- y
                m <<- NULL
        }
        get = function() x
        setinv = function(inverse) m <<- inverse 
        getinv = function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# cacheSolve computes the inverse of the special "matrix"
# returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        m = x$getinv()
        
        # if the inverse was already calculated then output a message:
        if (!is.null(m)){
                # get it from the cache 
                message("getting cached data")
                return(m)
        }
        
        # otherwise, calculate the inverse: 
        data = x$get()
        m = solve(data, ...) # the solve function of R to produce the inverse
        
        # set the value of the inverse
        x$setinv(m)
        return(m)
}

# Function named testx that measures the time the functions above take
# to show that there was time saved:

timer = function(mat){
               
        temp = makeCacheMatrix(mat)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}

# Tests for the code above:

test_1 <- makeCacheMatrix(matrix(1:4,2,2))
test_1$get()
test_1$getinv()
cacheSolve(test_1)

timer(matrix(1:4,2,2))

test_1 <- makeCacheMatrix(matrix(c(3, 9, -1, 4, 2, 6, 5, 7, 9), nrow = 3))
test_1$get()
test_1$getinv()
cacheSolve(test_1)

timer(matrix(c(3, 9, -1, 4, 2, 6, 5, 7, 9), nrow = 3))
