 #############################################################################
 #   Copyright (c) 2008 Christophe Dutang                                                                                                  #
 #                                                                                                                                                                        #
 #   This program is free software; you can redistribute it and/or modify                                               #
 #   it under the terms of the GNU General Public License as published by                                         #
 #   the Free Software Foundation; either version 2 of the License, or                                                   #
 #   (at your option) any later version.                                                                                                            #
 #                                                                                                                                                                         #
 #   This program is distributed in the hope that it will be useful,                                                             #
 #   but WITHOUT ANY WARRANTY; without even the implied warranty of                                          #
 #   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                 #
 #   GNU General Public License for more details.                                                                                    #
 #                                                                                                                                                                         #
 #   You should have received a copy of the GNU General Public License                                           #
 #   along with this program; if not, write to the                                                                                           #
 #   Free Software Foundation, Inc.,                                                                                                              #
 #   59 Temple Place, Suite 330, Boston, MA 02111-1307, USA                                                             #
 #                                                                                                                                                                         #
 #############################################################################
### pseudo random generation
###
###			R functions
### 


### set the seed ###

setSeed <- function(seed)
	invisible( .Call("doSetSeed", seed) )


### pseudo random generation ###

congruRand <- function(n, dim = 1, mod = 2^31-1, mult = 16807, incr = 0, echo)
{
        if(!is.numeric(n) || any(n <=0))
                stop("invalid argument 'n'")
        if(!is.numeric(dim) || length(dim) !=1 || any(dim <= 0))
                stop("invalid argument 'dim'")
        if(!is.numeric(mod) || length(mod) !=1)
                stop("invalid argument 'mod'")
        if(!is.numeric(mult) || length(mult) != 1 || mult > mod || mult < 0)
                stop("invalid argument 'mult'")
        if(!is.numeric(incr) || length(incr) != 1 || incr > mod || incr < 0)
                stop("invalid argument 'incr'")    
           
        if(missing(echo))
                echo <- FALSE
    
        if(length(n) > 1)
                res <- .Call("doCongruRand", length(n), dim, mod, mult, incr, echo)
        else
                res <- .Call("doCongruRand", n, dim, mod, mult, incr, echo)	
        if(dim == 1)    
                as.vector(res)
        else
                as.matrix(res)
}

SFMT <- function(n, dim = 1, mexp = 19937, usepset = TRUE, withtorus = FALSE, usetime = FALSE)
{    
        if(n <0 || is.array(n))
                stop("invalid argument 'n'")
        if(dim < 0 || length(dim) >1)
                stop("invalid argument 'dim'")
        if(!is.logical(withtorus) && !is.numeric(withtorus))
                stop("invalid argument 'withtorus'")
        if(!is.numeric(mexp))
                stop("invalid argument 'mexp'")
        if(!is.logical(usepset))
                stop("invalid argument 'usepset'")
        
        authorizedParam <- c(607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091)
        
        if( !(mexp %in% authorizedParam) )
                stop("'mexp' must be in {607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091}. ")
    
    
        if(!is.logical(withtorus))
        {
                if(0 < withtorus && withtorus <= 1)
                    nbTorus <- floor( withtorus * n )
                if(withtorus <=0 || withtorus > 1) 
                    stop("invalid argument 'withtorus'")
        }
        if(is.logical(withtorus))
        {   
                if(!withtorus)
                    nbTorus <- 0
                else
                    stop("invalid argument 'withtorus'")
        }
    
        if(nbTorus == 0)
        {
                if(length(n) > 1)
                        res <- .Call("doSFMersenneTwister", length(n), dim, mexp, usepset)
                else
                        res <- .Call("doSFMersenneTwister", n, dim, mexp, usepset)	
        }   
        else
        {
                restorus <- torus(nbTorus, dim, mixed = FALSE, usetime = usetime)
            
                if(length(n) > 1)
                        res <- .Call("doSFMersenneTwister", length(n) - nbTorus, dim, mexp, usepset)
                else
                        res <- .Call("doSFMersenneTwister", n- nbTorus, dim, mexp, usepset)
            
                res <- rbind(res, as.matrix( restorus, nbTorus, dim) )
        }
    
        if(dim == 1)
                as.vector(res)
        else
                as.matrix(res)
}
 
WELL <- function(n, dim = 1, order = 512, temper = FALSE, version = "a")
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
    

    
    if(dim == 1)
        as.vector(res)
    else
        as.matrix(res)
}

knuthTAOCP <- function(n, dim = 1)
{
    if(n <0 || is.array(n))
        stop("invalid argument 'n'")
    if(dim < 0 || length(dim) >1)
        stop("invalid argument 'dim'")
    
    if(length(n) > 1)
        res <- .Call("doKnuthTAOCP", length(n), dim)
    else
        res <- .Call("doKnuthTAOCP", n, dim)	
    
    if(dim == 1)
        as.vector(res)
    else
        as.matrix(res)
}

