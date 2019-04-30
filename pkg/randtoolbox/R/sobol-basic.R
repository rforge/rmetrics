## 
# @file  sobol-basic.R
# @brief R file implementing the basic Sobol recurrence without Gray code
#
# @author Christophe Dutang
#
#
# Copyright (C) 2017, Christophe Dutang, 
# All rights reserved.
#
# The new BSD License is applied to this software.
# Copyright (c) 2017 Christophe Dutang. 
# Christophe Dutang, see http://dutangc.free.fr
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
### Sobol
###
###			R functions
### 

int2bit <- function(x)
  as.numeric(intToBits(x))
bit2int <- function(x)
  sum(x * 2^(1:length(x)-1))
bit2unitreal <- function(x) #radical inverse function in base 2
  sum(x / 2^(1:length(x)))


#polycoef = (c_q, c_q-1, ..., c_2, c_1, 1) for q=deg with c_q=1 (always)
#prevmj = (m_j-1, m_j-2,..., m_j-q)
sobol.directions <- function(polycoef, deg, prevmj, nbpoint, echo=FALSE)
{
  stopifnot(is.matrix(prevmj))
  stopifnot(all(prevmj <= 1 & prevmj >= 0))
  
  mjrec <- function(prevmj)
  {
    if(echo)
      print(prevmj)
    if(deg > 0)
      stopifnot(NCOL(prevmj) == deg)
    mj <- prevmj[, 1]
    if(deg > 0)
      for(k in 1:deg)
      {
        if(polycoef[k] != 0)
        {
          if(echo)
          {
            cat("k", k, "2^(deg - k + 1)", 2^(deg - k + 1), "m_j-k",  bit2int(prevmj[,k]))
            cat(" 2^q-k", 2^(deg - k + 1), "\n")  
            cat("integer in rec", 2^(deg - k + 1) * polycoef[k] * bit2int(prevmj[,k]), "\n")
          }
          #mj = mj + m_j-k*2^(deg-k+1)*c_deg
          mj <- mj + int2bit(2^(deg - k + 1) * polycoef[k] * bit2int(prevmj[,k]))
          #modulo 2
          mj <- mj %% 2 
        }
      }
    mj
  }
  allmj <- prevmj
  if(nbpoint > 0)
    for(n in 1:nbpoint)
    {
      d <- NCOL(allmj)
      if(echo)
        print(allmj)
      if(deg > 0)
      {
        prevmj <- allmj[, (d-deg+1):d]
        nextmj <- mjrec(as.matrix(prevmj))
      }else
        nextmj <- allmj[,d]
      if(echo) cat("\n")
      allmj <- cbind(allmj, nextmj)
    }
  colnames(allmj) <- apply(allmj, 2, bit2int)
  
  if(NROW(allmj) < NCOL(allmj))
    print(allmj)
    
  #M contains m1,..., mj
  M <- allmj[1:NCOL(allmj), ]
  
  #compute direction numbers vj (in place)
  for(j in 2:NCOL(M))
    M[1:j, j] <- rev(M[1:j, j])
  return(M)
}


#output = real : x_j in [0,1)
#output = integer : y_j on which to compute the radical inverse
sobol.basic <- function(n, polycoef, deg, prevmj, echo=FALSE, output=c("real", "integer"))
{
  output <- match.arg(output, c("real", "integer"))
  
  if(is.vector(prevmj))
    prevmj <- sapply(prevmj, int2bit)
  else if(is.matrix(prevmj))
    stopifnot(all(prevmj <= 1 & prevmj >= 0))
  else
    stop("wrong prevmj")
  #maximum number of components for the binary representation
  r <- ceiling(log(n)/log(2)) 
  #compute direction numbers
  V <- sobol.directions(polycoef, deg, prevmj, r-deg, echo=FALSE)
  if(echo) print(V)
  #computer output reals or integers
  x <- numeric(n)
  for(i in 1:n)
  {
    #current integer by rec (modulo 2)
    y <- (V %*% int2bit(i)[1:r]) %% 2
    if(echo) 
    {
      cat("i\n")
      print(y)
    }
    if(output == "real") #convert in [0,1)
    {
      x[i] <- bit2unitreal(y)
    }else
      x[i] <- bit2int(y)
  }
  x
}


sobol.R <- function(n, d, echo=FALSE)
{
  stopifnot(d <= 20)
  #see LowDiscrepancy.f, line 550
  polynomvect <- c(1,3,7,11,13,19,25,37,59,47,61,55,41,67,97,91,
                   109,103,115,131,193,137,145,143,241,157,185,167,229,171,213,
                   191,253,203,211,239,247,285,369,299,301,333,351,355,357,361,
                   391,397,425,451,463,487,501,529,539,545,557,563,601,607,617,
                   623,631,637,647,661,675,677,687,695,701,719,721,731,757,761,
                   787,789,799,803,817,827,847,859,865,875,877,883,895,901,911,
                   949,953,967,971,973,981,985,995,1001,1019,1033,1051,1063,
                   1069,1125,1135,1153,1163,1221,1239,1255,1267,1279,1293,1305,
                   1315,1329,1341,1347,1367,1387,1413,1423,1431,1441,1479,1509,
                   1527,1531,1555,1557,1573,1591,1603,1615,1627,1657,1663,1673,
                   1717,1729,1747,1759,1789,1815,1821,1825,1849,1863,1869,1877,
                   1881,1891,1917,1933,1939,1969,2011,2035,2041,2053,2071,2091,
                   2093,2119,2147,2149,2161,2171,2189,2197,2207,2217,2225,2255,
                   2257,2273,2279,2283,2293,2317,2323,2341,2345,2363,2365,2373,
                   2377,2385,2395,2419,2421,2431,2435,2447,2475,2477,2489,2503,
                   2521,2533,2551,2561,2567,2579,2581,2601,2633,2657,2669)
  #see Glasserman, page 311
  mjmatrix <- cbind(1, #m1 
                    c(1,3), #m2 
                    c(1,5,7,7,5,1,3,3,7,5,5,7,7,1,3,3,7,5,1,1), #m3
                    c(1,15,11,5,3,1,7,9,13,11,1,3,7,9,5,13,13,11,3,15), #m4
                    c(1,17,13,7,15,9,31,9,3,27,15,29,21,23,19,11,25,7,13,17), #m5
                    c(1,51,61,43,51,59,47,57,35,53,19,51,61,37,33,7,5,11,39,63), #m6
                    c(1,85,67,49,125,25,109,43,89,69,113,47,55,97,3,37,83,103,27,13), #m7
                    c(1,255,79,147,141,89,173,43,9,25,115,97,19,97,197,101,255,29,203,65) #m8
                    )
  #maximum number of components for the binary representation
  r <- ceiling(log(n)/log(2)) 
  #compute direction numbers V
  V <- array(0, dim=c(r, r, d))
  for(i in 1:d)
  {
    pi <- int2bit(polynomvect[i])
    degpi <- max(pi * 0:31)
    if(degpi > 0)
      mji <- sapply(mjmatrix[i, 1:degpi], int2bit)
    else
      mji <- int2bit(1)
    mji <- as.matrix(mji)
    if(echo)
    { 
      cat("i=", i, "pi=", polynomvect[i], "deg", degpi, "\n")
      print(head(mji))
    }
    
    Vi <- sobol.directions(pi, degpi, mji, r-NCOL(mji), echo=FALSE)
    V[,,i] <- Vi
  }
  #compute reals
  
  x <- matrix(nrow=n, ncol=d)
  for(i in 1:d)
  for(j in 1:n)
  {
    #current integer by rec (modulo 2)
    y <- (V[,,i] %*% int2bit(j)[1:r]) %% 2
    x[j,i] <- bit2unitreal(y)
  }
  x
}


