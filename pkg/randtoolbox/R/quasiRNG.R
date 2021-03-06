## 
# @file  quasiRNG.R
# @brief R file for all quasi RNGs
#
# @author Christophe Dutang
# @author Diethelm Wuertz 
#
# Copyright (C) 2009, Diethelm Wuertz, ETH Zurich. 
# Copyright (C) 2020, Christophe Dutang, 
# Christophe Dutang, see http://dutangc.free.fr
# All rights reserved.
#
# The new BSD License is applied to this software.
# Copyright (c) 2019 Christophe Dutang, Diethelm Wuertz. 
# All rights reserved.
#
#      Redistribution and use in source and binary forms, with or without
#      modification, are permitted provided that the following conditions are
#      met:
#      
#          - Redistributions of source code must retain the above copyright
#          notice, this list of conditions and the following disclaimer.
#          - Redistributions in binary form must reproduce the above
#          copyright notice, this list of conditions and the following
#          disclaimer in the documentation and/or other materials provided
#          with the distribution.
#          - Neither the name of the ETH Zurich nor the names of its 
#          contributors may be used to endorse or promote 
#          products derived from this software without specific prior written
#          permission.
#     
#      THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#      "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#      LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#      A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#      OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#      SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#      LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#      DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#      THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#      (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#      OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#  
#
#############################################################################
### quasi random generation
###
###			R functions
### 


### quasi random generation ###

torus <- function(n, dim = 1, prime, init = TRUE, mixed = FALSE, usetime = FALSE, 
                  normal=FALSE, mexp = 19937)
{
  ## Check arguments
  if(is.array(n) || !is.numeric(n))
    stop("invalid argument 'n'")
  if(length(dim) >1)
    stop("invalid argument 'dim'")
  if(dim < 1 || dim > 100000)
    stop("invalid argument 'dim'")
  if(!is.logical(usetime))
    stop("invalid argument 'usetime'")
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
  ## Mersenne exponent only used when mixed=TRUE
  authorizedParam <- c(607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091)
  if( !(mexp %in% authorizedParam) )
    stop("'mexp' must be in {607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091}.")
  
  
  ## Restart Settings:
  if(init) 
    .setrandtoolboxEnv(.torus.seed = list(offset = 0))
  if(!exists(".torus.seed", envir=.randtoolboxEnv, mode="list"))
    stop("Torus algorithm not initialized.")
  
  ## Compute        
  nb <- ifelse(length(n)>1, length(n), n)
  if(nb < 0) stop("invalid argumet 'n'")
  if(nb == 0) return(numeric(0))
  startpt <- .getrandtoolboxEnv(".torus.seed")$offset
  
  #implemented in src/randtoolbox.c
  res <- .Call(CF_doTorus, nb, dim, prime, startpt, mixed, usetime, mexp)
  
  if(any(res > 1 | res < 0))
    warning("A call to torus() generate numerics outside (0,1).")
  
  ## Normal transformation
  if(normal)
    res <- qnorm(res)
  
  ## For the next numbers save (only used if init=FALSE in the next call)
  .setrandtoolboxEnv(.torus.seed = list(offset = startpt+nb))	
  
  ## Result	
  if(dim == 1)
    as.vector(res)
  else
    as.matrix(res)
}

get.primes <- function(n)
{
  n <- min(n,100000)
  .C("get_primes",as.integer(n),integer(n))[[2]]
}

#(n, dim = 1, prime, init = TRUE, usetime = FALSE, normal=FALSE)
halton <- function (n, dim = 1, init = TRUE, normal = FALSE, usetime = FALSE, 
                    mixed = FALSE, method="C", mexp = 19937)
{   
  # A function based on Diethelm Wuertz's code
  
  if(is.array(n) || !is.numeric(n))
    stop("invalid argument 'n'")
  if(length(dim) >1)
    stop("invalid argument 'dim'")   
  if(dim < 1 || dim > 100000)
    stop("invalid argument 'dim'")
  method <- match.arg(method, c("C", "Fortran"))
  if(!is.logical(usetime))
    stop("invalid argument 'usetime'")
  if(!is.logical(mixed))
    stop("invalid argument 'mixed'")
  
  # Description:
  #   Uniform Halton Low Discrepancy Sequence
  
  # Details:
  #   DIMENSION : dimension limited to 100000
  #       N : LD numbers to create
  
  # Initiate starting point:
  if(usetime)
    start <- as.numeric(Sys.time())
  else
  {
    start <- 0
  }
  
  # Restart Settings:
  if (init) 
    .setrandtoolboxEnv(.halton.seed = list(base = rep(0, dim), offset = start))
  if(!exists(".halton.seed", envir=.randtoolboxEnv, mode="list"))
    stop("Halton algorithm not initialized.")
  
  nb <- ifelse(length(n)>1, length(n), n)
  if(nb < 0) stop("invalid argumet 'n'")
  if(nb == 0) return(numeric(0))
  rngEnv <- .getrandtoolboxEnv(".halton.seed")
  
  if(method == "Fortran")
  {  
    # Generate:
    qn <- numeric(nb * dim)
    
    # SUBROUTINE HALTON_F(QN, N, DIMEN, BASE, OFFSET, INIT, TRANSFORM)
    #implemented in src/LowDiscrepancy.f
    result_fortran <- .Fortran(CF_halton_f,
                       qn= as.double( qn ),
                       n= as.integer( nb ),
                       dim= as.integer( dim ),
                       base= as.integer( rngEnv$base ),
                       offset= as.integer( rngEnv$offset ),
                       init= as.integer( init ),
                       trans= as.integer( 0 ),
                       PACKAGE = "randtoolbox")
    
    # For the next numbers save (only used if init=FALSE in the next call)
    .setrandtoolboxEnv(.halton.seed = result_fortran[c("base", "offset")])
    # Deviates:
    result <- matrix(result_fortran[["qn"]], ncol = dim)
    
    #add 0 for starting point, decrease offset (eventually used if init=FALSE)
    if(!usetime && init) 
    {
      if(dim == 1)
        result <- c(0, result[-nb])
      else
        result <- rbind(0, result[-nb,])
      lshift <- list(base = result_fortran[["base"]], offset=result_fortran[["offset"]]-1)
      .setrandtoolboxEnv(.halton.seed = lshift)
    }
    
  }else #method == "C"
  {
    ## Mersenne exponent only used when mixed=TRUE
    authorizedParam <- c(607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091)
    if( !(mexp %in% authorizedParam) )
      stop("'mexp' must be in {607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091}. ")
    
    #implemented in src/randtoolbox.c
    result <- .Call(CF_doHalton, nb, dim, rngEnv$offset, mixed, usetime, mexp)
    # For the next numbers save (only used if init=FALSE in the next call)
    .setrandtoolboxEnv(.halton.seed = list("base"=get.primes(dim), "offset"=rngEnv$offset+nb))
  }
  
  if(any(result > 1 | result < 0))
    warning("A call to halton() generate numerics outside (0,1).")
  
  ## Normal transformation
  if(normal)
    result <- qnorm(result)
  
  # Return Value:
  if(dim == 1)
    as.vector(result)
  else
    as.matrix(result)
}


runif.halton <- halton


sobol <- function (n, dim = 1, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                   mixed = FALSE, method="Fortran", mexp = 19937)
{   
  # A function implemented by Diethelm Wuertz
  if(is.array(n) || !is.numeric(n))
    stop("invalid argument 'n'")
  if(length(dim) >1)
    stop("invalid argument 'dim'")    
  if(dim < 1 || dim > 1111) #prepare the future release
    stop("invalid argument 'dim'")
  if( !any(scrambling == 0:3) )
    stop("invalid argument 'scrambling'")    
  method <- match.arg(method, c("C", "Fortran"))
  maxit <- 100 #for scrambled sequences when sobol_fortran() generates numbers outside [0,1)
  
  nb <- ifelse(length(n)>1, length(n), n)
  if(nb < 0) stop("invalid argumet 'n'")
  if(nb == 0) return(numeric(0))
  
  scramblmixed <- scrambling > 0 || mixed
  
  if(scrambling > 0 && mixed)
    warnings("only scrambling is used.")
  if(scrambling > 0)
  {
    if(is.null(seed))
      seed <- 4711 #default value
    else
      seed <- as.integer(seed) #convert it to integer
  }else if(scrambling == 0)
  {
    if(mixed)
      seed <- as.integer((2^32-1)*runif(1))
    else
      seed <- 0 #default value for non-mixed SFMT
  }
  
  if(method == "Fortran")
  {  
    # Description:
    #   Uniform Sobol Low Discrepancy Sequence
    # Details:
    #   DIMENSION : dimension <= 1111
    #           N : LD numbers to create
    #  SCRAMBLING : One of the numbers 0,1,2,3
    
    
    # Restart Settings:
    if (init) 
      .setrandtoolboxEnv(.sobol.seed = 
                           list(quasi = rep(0, dim), ll = 0, count = 0, sv = rep(0, dim*30), seed = seed))
    if(!exists(".sobol.seed", envir=.randtoolboxEnv, mode="list"))
      stop("Sobol algorithm not initialized.")
    
    if(!init) #starting from last point
    {
      # Generate:
      qn <- numeric(nb * dim)
      
      #  SUBROUTINE SOBOL_F(QN, N, DIMEN, QUASI, LL, COUNT, SV, IFLAG, iSEED, INIT, TRANSFORM)
      #implemented in src/LowDiscrepancy.f
      result <- .Fortran(CF_sobol_f,
                         as.double( qn ),
                         as.integer( nb ),
                         as.integer( dim ),
                         as.double ( .getrandtoolboxEnv(".sobol.seed")$quasi ),
                         as.integer( .getrandtoolboxEnv(".sobol.seed")$ll ),
                         as.integer( .getrandtoolboxEnv(".sobol.seed")$count ),
                         as.integer( .getrandtoolboxEnv(".sobol.seed")$sv ),
                         as.integer( scrambling ),
                         as.integer( .getrandtoolboxEnv(".sobol.seed")$seed ),
                         as.integer( init ),
                         as.integer( 0 ),
                         PACKAGE = "randtoolbox")
      F_sobol <- TRUE
      # Deviates:
      sobolres <- matrix(result[[1]], ncol = dim)
    }else # start from 0
    {
      if(nb > 1)
      {
        # Generate nb-1 points
        qn <- numeric((nb-1) * dim)
        
        #  SUBROUTINE SOBOL_F(QN, N, DIMEN, QUASI, LL, COUNT, SV, IFLAG, iSEED, INIT, TRANSFORM)
        #implemented in src/LowDiscrepancy.f
        result <- .Fortran(CF_sobol_f,
                           as.double( qn ),
                           as.integer( nb-1 ),
                           as.integer( dim ),
                           as.double ( .getrandtoolboxEnv(".sobol.seed")$quasi ),
                           as.integer( .getrandtoolboxEnv(".sobol.seed")$ll ),
                           as.integer( .getrandtoolboxEnv(".sobol.seed")$count ),
                           as.integer( .getrandtoolboxEnv(".sobol.seed")$sv ),
                           as.integer( scrambling ),
                           as.integer( .getrandtoolboxEnv(".sobol.seed")$seed ),
                           as.integer( init ),
                           as.integer( 0 ),
                           PACKAGE = "randtoolbox")
        F_sobol <- TRUE
        # Deviates:
        sobolres <- matrix(result[[1]], ncol = dim)
        #add 0 for starting point
        if(dim == 1)
          sobolres <- c(0, sobolres)
        else
          sobolres <- rbind(0, sobolres)
      }else #sequence is only 0
      {
        F_sobol <- FALSE
        sobolres <- matrix(0, ncol=dim)
      }
     }
    
    if(scramblmixed)
    {  
      if(any(sobolres >= 1 | sobolres < 0))
      {
        warning("A call to sobol() generate numerics outside [0,1), so seed is randomized.")
        
        iter <- 0
        while(any(sobolres >= 1 | sobolres < 0) && iter < maxit)
        {
          iter <- iter + 1
          myrandseed <- (2^32-1)*runif(1)
          result <- .Fortran(CF_sobol_f,
                             as.double( qn ),
                             as.integer( nb ),
                             as.integer( dim ),
                             as.double ( .getrandtoolboxEnv(".sobol.seed")$quasi ),
                             as.integer( .getrandtoolboxEnv(".sobol.seed")$ll ),
                             as.integer( .getrandtoolboxEnv(".sobol.seed")$count ),
                             as.integer( .getrandtoolboxEnv(".sobol.seed")$sv ),
                             as.integer( scrambling ),
                             as.integer( myrandseed ),
                             as.integer( init ),
                             as.integer( 0 ),
                             PACKAGE = "randtoolbox")
          sobolres <- matrix(result[[1]], ncol = dim)
        }
        if(iter == maxit)
          stop("100 calls to sobol() have all generated (some) numerics outside (0,1), so we resign.")
        #=> else appropriate seed found
        
      }
    }else
    {
      if(any(sobolres >= 1 | sobolres < 0))
        warning("A call to sobol() generate numerics outside [0,1).")
    }
    
    # For the next numbers save (only used if init=FALSE in the next call)
    if(F_sobol) 
      .setrandtoolboxEnv(.sobol.seed = list(quasi = result[[4]], ll = result[[5]],
                                                count = result[[6]], sv = result[[7]], 
                                                seed = result[[9]]))
  }else #method == "C"
  {
    ## Mersenne exponent only used when mixed=TRUE
    authorizedParam <- c(607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091)
    if( !(mexp %in% authorizedParam) )
      stop("'mexp' must be in {607, 1279, 2281, 4253, 11213, 19937, 44497, 86243, 132049, 216091}. ")
    stop("sobol in C not yet implemented")
    
    sobolres <- .Call(CF_doSobol, nb, dim, 0, FALSE, FALSE, mexp)
    
  }
  
  ## Normal transformation
  if(normal)
    sobolres <- qnorm(sobolres)   
  
  # Return Value:
  if(dim == 1)
    as.vector(sobolres)
  else
    as.matrix(sobolres)    
}


runif.sobol <- sobol

