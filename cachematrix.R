## isSourceSet        - checks if the input has a valid source matrix
##
## makeCacheMatrix    - creates a list of four functions
##                      that operates the cache for the matrix
##
## cacheSolve         - takes a matrix argument and checks the cache for the
##                      presence of inverse solution before computing it
##	        	thereby optimizing on compute cycles
##
## *** WARNING *** 
##
## IN this implementation the setSolved cache operator function inside 
## makeCacheMatrix operates on trust and does not verify if the set value 
## is indeed the inverse of the source matrix

## FUNCTION 1 - isSourceSet
## isSourceSet expects a matrix as an argument and returns FALSE if
## the argument is a 1 x 1 matrix with NA in it

isSourceSet <- function(source) {

##	if (!is.matrix(source))stop("source must be a matrix")

	## Default matrix() argument is 1x1 matrix with NA data
	## If above condition is matched return FALSE else return TRUE

	nr <<- nrow(source)
	nc <<- ncol(source)
	if((nr == nc) && (nr == 1) && is.na(source)) FALSE else TRUE
}

## FUNCTION 2 - makeCacheMatrix
##
## makeCacheMatrix creates a special list of four functions as below.
## These four functions operate the cache. Since the getSolved function
## will be most frequently used, it has to be a high performance function
##
## 1. set the matrix source data in the cache
## 2. get the matrix source data from the cache
## 3. set the value of the inverse of the matrix in the cache
## 4. get the value of the inverted matrix stored in the cache

makeCacheMatrix <- function(cacheMatrix = matrix()) {

	cachedSolution <- NULL
	## Check if we were called with an argument
	if(isSourceSet(cacheMatrix)) {

		## Make sure that input we are getting has an inverse
		if((class(try(m <- solve(cacheMatrix),silent=T))=="matrix") == FALSE)
			stop("sourceData does not have inverse")

		## We did get an inverse. So store result in cachedSolution
		cachedSolution <- m
	}

	## the set function copies and stores the original matrix 
	## into cacheMatrix
	## 
	## the function checks if the input matrix is solvable and
	## in the process, sets the inverse computed into
	## the cache
	##
	## if the matrix does not have an inverse, the function throws
	## an error and stops

	set <- function (sourceData) {

   		## Make sure sourceData is of type "matrix"
		if(!is.matrix(sourceData)) stop("argument to set must be a matrix")

		## If we are getting a 1x1 matrix with NA in it, set
		## cachedSolution to NULL
		##
		##

		if(!isSourceSet(sourceData))
		{
			cachedSolution <- NULL
			return(cacheMatrix <- matrix())
		}
		## Make sure that input we are getting has an inverse
		else if ((class(try(m <- solve(sourceData),silent=T))=="matrix") == FALSE)
		{
			## If set fails the source and solution are both reset
			## to a 1x1 NA matrix and NULL respectively
			cachedSolution <- NULL
			cacheMatrix <- matrix()
			stop("sourceData does not have inverse")
		}

		cachedSolution <<- m
		cacheMatrix <<- sourceData
	}

	## the get function simply returns the stored matrix

	get <- function() cacheMatrix

	## the setSolved function takes a matrix argument and stores
	## it into the cached solution.
	##
	## it checks if the functions argument is indeed a matrix
	##
	## WARNING: No checks are made to see if the solution presented
	## is indeed the inverse of the cached original matrix

	setSolved <- function (solution) {

		## Is the source data valid?
		## If source is not set, we will not allow a solution
		## to be set

		if(!isSourceSet(cacheMatrix))
			stop("setting solution on uninitialized source is unacceptable")

   		## Make sure solution is of type "matrix"
		if(!is.matrix(solution)) stop("argument to setSolved must be a matrix")

		## ** WARNING ** No attempt is made to validate the solution
		## to see if it is the inverse of the source

		cachedSolution <<- solution
	}

	## the getSolved function simply returns the cachedSolution
	## It could be NULL if not set through  set or setSolved yet

	getSolved <- function() cachedSolution

	## the setNULLSolution is a test function return to invalidate
	## the cache

	setNULLSolution <- function() cachedSolution <<- NULL

	## Create the list of functions and return the list

	list ( set = set, get = get, setSolved = setSolved, getSolved = getSolved, setNULLSolution = setNULLSolution)
}


## FUNCTION 3 - cacheSolve
##
## This function requires a cacheMatrix object as its first argument, 
## the ellipsis passes the rest of arguments to the solve function
##
## If the cache has the inverse stored in it, this function returns
## the inverse matrix taken from the cache. Otherwise, it computes the inverse,
## stores the solution to the cache, before returning the vaue

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


	## call the getSolved function of the cacheMatrix x
	m <- x$getSolved()

	## if getSolved returns NULL, then the cache is empty

	if(!is.null(m)) {
		message ("getting cached data")

		## We are getting Non NULL data from cache. We are
		## good to return that value
		return(m)
	}

	## Get the source matrix from the cache

	matr <- x$get()

	## Compute the inverse of the solution

	inverse <- solve(matr, ...)

	## Set the inverse value in cache using the setSolved function
	x$setSolved(inverse)

	## return the inverse matrix
	inverse
}
