## 
# @file  qmc.R
# @brief R file for test
#
# @author Christophe Dutang
#
#
# Copyright (C) 2009, Christophe Dutang, 
# All rights reserved.
#
# The new BSD License is applied to this software.
# Copyright (c) 2009 Christophe Dutang. 
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
#          - Neither the names of its contributors may be used to endorse or promote 
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
###  QMC and MC applications
###
###			R codes 
### 
### 
###     to use it uncomment all the following lines with your favourite editor.

# require(fExoticOptions)
# require(randtoolbox)



# # comparison of QMC and MC methods for a Down Out Call
# compBarrier <- function(nbsimumax, nbsimupoint, nbpointdiscr)
# {
#     #--- parameters
#     asset_t0 <- 100
#     r <- 5/100
#     sigma <- 20/100
#     T <- 1
#     H <- 50 
#     X <- 50
#     K <- 0
#     steptime <- T/nbpointdiscr
#     
#     theoprice <- StandardBarrierOption("cdo", asset_t0, X, H, K, T, r, r, sigma)@price
#     
#     nbsimu <- seq(1, nbsimumax, len=nbsimupoint)
#     
#     Toruserror <- vector("numeric", nbsimupoint)
#     SFMTerror <- vector("numeric", nbsimupoint)
#     PKerror<- vector("numeric", nbsimupoint)
#     
#     for(k in 1:nbsimupoint)
#     {
#         #--- with SF Mersenne Twister
#         
#         asset_ti_1 <- rep(asset_t0, nbsimu[k])
#         activate <- asset_ti_1 > H
#         
#         SFMTunif <- SFMT( nbsimu[k], nbpointdiscr )      
#         
#         for(i in 1:nbpointdiscr)
#         {
#             asset_ti <- asset_ti_1 * exp( (r-sigma^2/2) * steptime + sigma * sqrt(steptime) * qnorm( SFMTunif[ ,i] ) )
#             activate <- activate & ( asset_ti > H)
#             asset_ti_1 <- asset_ti
#         }
#         
#         rm(SFMTunif)
#         SFMTerror[k] <- ( mean( pmax(asset_ti - X, 0) * activate * exp(-r*T) ) - theoprice )/ theoprice
#         
#        #--- with Torus algorithm
#         
#         asset_ti_1 <- rep(asset_t0, nbsimu[k])
#         activate <- asset_ti_1 > H
#         
#         Torusunif <- torus( nbsimu[k] , nbpointdiscr, use=FALSE)
#         
#         for(i in 1:nbpointdiscr)
#         {
#             asset_ti <- asset_ti_1 * exp( (r-sigma^2/2) * steptime + sigma * sqrt(steptime) * qnorm( Torusunif[ ,i] ) )
#             activate <- activate & ( asset_ti > H)
#             asset_ti_1 <- asset_ti
#         }
#         
#         rm(Torusunif)
#         Toruserror[k] <- ( mean( pmax(asset_ti - X, 0) * activate * exp(-r*T) ) - theoprice )/ theoprice
#         
#        #--- with Park Miller sequence
#         
#         asset_ti_1 <- rep(asset_t0, nbsimu[k])
#         activate <- asset_ti_1 > H
#         
#         PKunif <- congruRand( nbsimu[k] , nbpointdiscr)
#         
#         for(i in 1:nbpointdiscr)
#         {
#             asset_ti <- asset_ti_1 * exp( (r-sigma^2/2) * steptime + sigma * sqrt(steptime) * qnorm( PKunif[ ,i] ) )
#             activate <- activate & ( asset_ti > H)
#             asset_ti_1 <- asset_ti
#         }
#         
#         rm(PKunif)
#         PKerror[k] <- ( mean( pmax(asset_ti - X, 0) * activate * exp(-r*T) ) - theoprice )/ theoprice
#         
#         
#     }
#     limits <- c(-.02, .02)
#     legtxt <- c("SFMT","Torus","Park Miller")
#     legcol <- c("red","black","blue")
#     title <- "Down Out Call"
#     
#     
#     plot(nbsimu, SFMTerror, t='l', col="red", ylim=limits, xlab="simulation number", ylab="relative error", main=title) 
#     lines(nbsimu, Toruserror, col = "black")
#     lines(nbsimu, PKerror, col = "blue")
#     
#     lines(nbsimu, rep(0, nbsimupoint), col="black", lty=2)
#     if(limits[2] > 0.5 || limits[2]+limits[1] == 0)
#         legend("topright",leg= legtxt, col=legcol , lty=1  )
#     if(limits[1] < -0.5 &&  limits[2] < 0.5)
#         legend("bottomright",leg= legtxt, col= legcol, lty=1  )
#     
# }

# #compBarrier(100000, 101, 250)

# # comparison of QMC and MC methods for a vanilla Call
# compVanilla <- function(nbmax, nbsimupoint)
# {
#     #--- parameters
#     asset1_t0 <- 100
#     r <- 5/100
#     sigma <- 20/100
#     
#     T <- 1
#     K <- 60
#     
#     theoprice <- GBSOption("c", asset1_t0, K, T, r, r, sigma)@price
#     
#     nbsimu <- seq(2,nbmax, len=nbsimupoint)
#     
#     Toruserror <- vector("numeric", nbsimupoint)
#     SFMTerror <- vector("numeric", nbsimupoint)
#     PKerror <- vector("numeric", nbsimupoint)    
#     
#     for(i in 1:nbsimupoint)
#     {
#         #--- with Torus algorithm
#         GaussRand <- qnorm( torus( nbsimu[i] ) )
#         asset_T <- asset1_t0 * exp( (r-sigma^2/2)*T + sigma*sqrt(T) * GaussRand)
#         
#         approxprice <- mean( pmax( asset_T - K ,0) * exp(-r*T) )
#         Toruserror[i] <- ( approxprice - theoprice ) / theoprice
#                 
#         #--- with SF-Mersenne Twister 
#         
#         GaussRand <- qnorm( SFMT( nbsimu[i] ) )
#         asset_T <- asset1_t0 * exp( (r-sigma^2/2)*T + sigma*sqrt(T) * GaussRand)
#         
#         approxprice <- mean( pmax( asset_T - K ,0 ) * exp(-r*T) )
#         SFMTerror[i] <- ( approxprice - theoprice ) / theoprice
#         
#         #--- with Park Miller sequence
#         
#         GaussRand <- qnorm( congruRand( nbsimu[i] ) )
#         asset_T <- asset1_t0 * exp( (r-sigma^2/2)*T + sigma*sqrt(T) * GaussRand)
#         
#         approxprice <- mean( pmax( asset_T - K ,0 ) * exp(-r*T) )
#         PKerror[i] <- ( approxprice - theoprice ) / theoprice
#     }
#             
#     limits <- c(-.02, .02)
#     legtxt <- c("SFMT","Torus","Park Miller")
#     legcol <- c("red","black","blue")
#     title <- "Vanilla Call"
#     
#     plot(nbsimu, SFMTerror, t='l', col="red", ylim=limits, xlab="simulation number", ylab="relative error", main=title) 
#     lines(nbsimu, Toruserror, col = "black")
#     lines(nbsimu, PKerror, col = "blue")
# #    lines(nbsimu, rep(0, nbsimupoint), col="black", lty=2)
#     
#     if(limits[2] > 0.5 || limits[2]+limits[1] == 0)
#         legend("topright",leg= legtxt, col=legcol , lty=1  )
#     if(limits[1] < -0.5 &&  limits[2] < 0.5)
#         legend("bottomright",leg= legtxt, col= legcol, lty=1  )    
# }

# #compVanilla(100000, 101)

