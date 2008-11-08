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
### quasi random generation
###
###			R functions
### 


### quasi random generation ###

torus <- function(n, dim = 1, prime, init = TRUE, mixed = FALSE, usetime = FALSE, normal=FALSE)
{
## Check arguments
        if(n <0 || is.array(n) || !is.numeric(n))
                stop("invalid argument 'n'")
        if(dim < 0 || length(dim) >1)
                stop("invalid argument 'dim'")
        if(!is.logical(usetime))
                stop("invalid argument 'mixed'")
        if(!is.logical(mixed))
                stop("invalid argument 'mixed'")
    
        if(missing(prime)) 
                prime <- NULL
        else
        {
                if(any(prime < 0) || !is.vector(prime))
                        stop("invalid argument 'prime'")

                dim <- length(prime)
                prime <- as.integer( prime )
        }
    
## Restart Settings:
    if(init) 
        .setrandtoolboxEnv(.torus.seed = list(offset = 1))
    
## Compute        
    nb <- ifelse(length(n)>1, length(n), n)
    startpt <- .getrandtoolboxEnv(".torus.seed")$offset
    
    res <- .Call("doTorus", nb, dim, prime, startpt, mixed, usetime)
## Normal transformation
    if(normal)
        res <- qnorm(res)
    
## For the next numbers save:
    .setrandtoolboxEnv(.torus.seed = list(offset = startpt+nb))
    
## Result	
        if(dim == 1)
                as.vector(res)
        else
                as.matrix(res)
}


halton <- function (n, dim = 1, init = TRUE, normal = FALSE, usetime = FALSE)
{   
# A function implemented by Diethelm Wuertz
    
    if(n <0 || is.array(n) || !is.numeric(n))
        stop("invalid argument 'n'")
    if(dim < 0 || dim > 200 || length(dim) >1)
        stop("invalid argument 'dim'")    
    
    
# Description:
#   Uniform Halton Low Discrepancy Sequence
    
# Details:
#   DIMENSION : dimension <= 200
#       N : LD numbers to create
    
# FUNCTION:
    if(usetime)
        start <- as.numeric(Sys.time())
    else
        start <- 0
    
# Restart Settings:
## YC : previous code should not needed since we have now global Env
    if (init) 
        .setrandtoolboxEnv(.runif.halton.seed = list(base = rep(0, dim), offset = start))
    
    
# Generate:
    qn = rep(0, ifelse(length(n)>1, length(n), n) * dim)
    
# SUBROUTINE HALTON(QN, N, DIMEN, BASE, OFFSET, INIT, TRANSFORM)
    if( !normal )
        result = .Fortran("halton",
                          as.double( qn ),
                          as.integer( ifelse(length(n)>1, length(n), n) ),
                          as.integer( dim ),
                          as.integer( .getrandtoolboxEnv(".runif.halton.seed")$base ),
                          as.integer( .getrandtoolboxEnv(".runif.halton.seed")$offset ),
                          as.integer( init ),
                          as.integer( 0 ),
                          PACKAGE = "randtoolbox")
    else
        result = .Fortran("halton",
                          as.double( qn ),
                          as.integer( ifelse(length(n)>1, length(n), n) ),
                          as.integer( dim ),
                          as.integer( .getrandtoolboxEnv(".runif.halton.seed")$base ),
                          as.integer( .getrandtoolboxEnv(".runif.halton.seed")$offset ),
                          as.integer( init ),
                          as.integer( 1 ),
                          PACKAGE = "randtoolbox")
    
        
# For the next numbers save:
    .setrandtoolboxEnv(.runif.halton.seed = list(base = result[[4]], offset = result[[5]]))
    
# Deviates:
    result = matrix(result[[1]], ncol = dim)
    
# Return Value:
    if(dim == 1)
        as.vector(result)
    else
        as.matrix(result)
}


runif.halton <- halton


sobol <- function (n, dim = 1, init = TRUE, scrambling = 0, seed = 4711, normal = FALSE)
{   
# A function implemented by Diethelm Wuertz
    if(n <0 || is.array(n) || !is.numeric(n))
        stop("invalid argument 'n'")
    if(dim < 0 || dim > 1111 || length(dim) >1)
        stop("invalid argument 'dim'")    
    if( !any(scrambling == 0:3) )
        stop("invalid argument 'scrambling'")    

    
# Description:
#   Uniform Sobol Low Discrepancy Sequence
    
# Details:
#   DIMENSION : dimension <= 1111
#           N : LD numbers to create
#  SCRAMBLING : One of the numbers 0,1,2,3
#
    
# FUNCTION:

    
# Restart Settings:
    if (init) 
        .setrandtoolboxEnv(.runif.sobol.seed = list(quasi = rep(0, dim), ll = 0,
                                                 count = 0, sv = rep(0, dim*30), seed = seed))
    
# Generate:
    qn = rep(0.0, ifelse(length(n)>1, length(n), n) * dim)
    
# SSOBOL(QN,N,DIMEN,QUASI,LL,COUNT,SV,IFLAG,SEED,INIT,TRANSFORM)
    if( !normal )
        result = .Fortran("sobol",
                          as.double( qn ),
                          as.integer( ifelse(length(n)>1, length(n), n) ),
                          as.integer( dim ),
                          as.double ( .getrandtoolboxEnv(".runif.sobol.seed")$quasi ),
                          as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$ll ),
                          as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$count ),
                          as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$sv ),
                          as.integer( scrambling ),
                          as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$seed ),
                          as.integer( init ),
                          as.integer( 0 ),
                          PACKAGE = "randtoolbox")
    else
        result = .Fortran("sobol",
                          as.double( qn ),
                          as.integer( ifelse(length(n)>1, length(n), n) ),
                          as.integer( dim ),
                          as.double ( .getrandtoolboxEnv(".runif.sobol.seed")$quasi ),
                          as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$ll ),
                          as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$count ),
                          as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$sv ),
                          as.integer( scrambling ),
                          as.integer( .getrandtoolboxEnv(".runif.sobol.seed")$seed ),
                          as.integer( init ),
                          as.integer( 1 ),
                          PACKAGE = "randtoolbox")
    
    
# For the next numbers save:
    .setrandtoolboxEnv(.runif.sobol.seed = list(quasi = result[[4]], ll = result[[5]],
                                             count = result[[6]], sv = result[[7]], seed = result[[9]]))
    
# Deviates:
    result = matrix(result[[1]], ncol = dim)
    
# Return Value:
    if(dim == 1)
        as.vector(result)
    else
        as.matrix(result)    
}


runif.sobol <- sobol

