 #############################################################################
 #   Copyright (c) 2009 Christophe Dutang and Petr Savicky                   #
 #                                                                           #
 #   This code can be used freely for personal, academic, or                 #
 #   non-commercial purposes. For commercial purposes, please contact        #
 #   P. L'Ecuyer at: lecuyer@iro.UMontreal.ca                                #
 #   (at your option) any later version.                                     #
 #                                                                           #
 #                                                                           #
 #############################################################################
### pseudo random generation
###
###			R functions
### 


### set the seed ###

setSeed4WELL <- function(seed)
	invisible( .Call("doSetSeed4WELL", seed) )


### pseudo random generation ###
 
WELL2test <- function(n, dim = 1, order = 512, temper = FALSE, version = "a")
{
    if(n <0 || is.array(n))
        stop("invalid argument 'n'")
    if(dim < 0 || length(dim) >1)
            stop("invalid argument 'dim'")
    if(!is.numeric(order))
            stop("invalid argument 'order'")
    if( !(order %in% c(512, 521, 607, 800, 1024, 19937, 21701, 23209, 44497) ) )
            stop("'order' must be in {512, 521, 607, 800, 1024, 19937, 21071, 23209, 44497}.")
    if( !(version %in% c("a", "b") ) )
            stop("'version' must be either 'a' or 'b'.")

    if(!is.logical(temper))
        stop("invalid argument 'temper'")
    if(temper && order %in% c(512, 521, 607, 1024))
        stop("tempering impossible")
    
    zeversion <- 0
    if(version == "a")
        zeversion <- 1
    if(version == "b")
        zeversion <- 2
    if(zeversion == 0)
        stop("wrong version for WELL RNG")
    if(version == "b" && order %in% c(512,  21701) ) 
        stop("this WELL RNG does not have a 'b' version")
    
    if(length(n) > 1)
        res <- .Call("doWELL", length(n), dim, order, temper, zeversion)
    else
        res <- .Call("doWELL", n, dim, order, temper, zeversion)	
    
	cat("Warning: you should use the function WELL from the randtoolbox package.\n")
    
    if(dim == 1)
        as.vector(res)
    else
        as.matrix(res)
}

