
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTIONS:            STABLE DISTRIBUTION:
#  .rsymstb              Returns random variates for symmetric stable DF
#  .symstb               Returns symmetric alpha-stable pdf/cdf
################################################################################


.rsymstb = 
function(n, alpha) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns random variates for symmetric stable DF
    
    # Details:
    #   Return random deviates from the stable family 
    #   of probability distributions. The results of 
    #   Chambers, Mallows, and Stuck is used.

    # FUNCTION:
    
    # Calculate uniform and exponential distributed random numbers:
    theta = pi * (runif(n)-1/2)
    w = -log(runif(n))
    
    # Calculate Random Deviates:
    if (alpha == 1) {
        result = rcauchy(n) 
    } else { 
        result = (sin(alpha*theta) / ((cos(theta))^(1/alpha))) *
            (cos((1-alpha)*theta)/w)^((1-alpha)/alpha)
    } 
    
    # Add Attribute:
    ans = result
    attr(ans, "control") = 
        cbind.data.frame(dist = "symstb", alpha = alpha, row.names = "")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.symstb =
function(x, alpha)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns symmetric alpha-stable pdf/cdf
    
    # Note: 
    #   symstb - returns symmetric alpha-stable pdf/cdf. The function  
    #   implements J.H. McCulloch's Fortran program for symmetric 
    #   distributions. Mc Cullochs approach has a density precision of 
    #   0.000066 and a distribution precision of 0.000022 for alpha in  
    #   the range [0.84, 2.00]. We have added only first order tail 
    #   approximation to calculate the tail density and probability.
    #   This has still to be improved!
    
    # FUNCTION:
    
    # Settings:
    X = prob = dens = x
    N = length(x)
    ei = 1:3
    u = rep(1, 3)
    q = rep(0, times = 6)
    p = matrix(rep(0, 120), ncol = 20)
    pd = matrix(rep(0, 100), ncol = 20)
    r = znot = zn4 = zn5 = rep(0, 19)
    zji = matrix(rep(0, 114), ncol = 6)
    combo = c(1, 5, 10, 10, 5, 1)
    s = matrix(c(                   
         1.8514190959e2, -4.6769332663e2,  4.8424720302e2, -1.7639153404e2,               
        -3.0236552164e2,  7.6351931975e2, -7.8560342101e2,  2.8426313374e2,               
         4.4078923600e2, -1.1181138121e3,  1.1548311335e3, -4.1969666223e2,               
        -5.2448142165e2,  1.3224487717e3, -1.3555648053e3,  4.8834079950e2,               
         5.3530435018e2, -1.3374570340e3,  1.3660140118e3, -4.9286099583e2,               
        -4.8988957866e2,  1.2091418165e3, -1.2285872257e3,  4.4063174114e2,               
         3.2905528742e2, -7.3211767697e2,  6.8183641829e2, -2.2824291084e2,               
        -2.1495402244e2,  3.9694906604e2, -3.3695710692e2,  1.0905855709e2,               
         2.1112581866e2, -2.7921107017e2,  1.1717966020e2,  3.4394664342e0,               
        -2.6486798043e2,  1.1999093707e2,  2.1044841328e2, -1.5110881541e2,               
         9.4105784123e2, -1.7221988478e3,  1.4087544698e3, -4.2472511892e2,               
        -2.1990475933e3,  4.2637720422e3, -3.4723981786e3,  1.0174373627e3,               
         3.1047490290e3, -5.4204210990e3,  4.2221052925e3, -1.2345971177e3,               
        -5.1408260668e3,  1.1090264364e4, -1.0270337246e4,  3.4243449595e3,               
         1.1215157876e4, -2.4243529825e4,  2.1536057267e4, -6.8490996103e3,               
        -1.8120631586e4,  3.1430132257e4, -2.4164285641e4,  6.9126862826e3,               
         1.7388413126e4, -2.2108397686e4,  1.3397999271e4, -3.1246611987e3,               
        -7.2435775303e3,  4.3545399418e3,  2.3616155949e2, -7.6571653073e2,               
        -8.7376725439e3,  1.5510852129e4, -1.3789764138e4,  4.6387417712e3),  
    byrow = FALSE, ncol = 19)             
                      
    # Setup:
    ca = gamma(alpha)*sin(pi*alpha/2)/pi
    sqpi = sqrt(pi)
    a2 = sqrt(2)-1
    cpxp0 = 1 / pi
    gpxp0 = 1 / (4*a2*sqpi)
    cpxpp0 = 2 * cpxp0
    gpxpp0 = 1.5 * gpxp0
    cppp = cpxpp0*3 - 2/pi
    gppp = gpxpp0*2.5 - 1/(32*sqpi*a2^3)
    znot = (1:19)*0.05
    zn4 = (1-znot)^4
    zn5 = (1-znot)*zn4   
    for (j in 1:19) 
        for (i in 0:5) 
            zji[j, i+1] = combo[i+1] * (-znot[j])^(5-i)        
    a = 2^(1/alpha)-1
    sp0 = gamma(1/alpha)/(pi*alpha)
    sppp0 = -gamma(3/alpha)/(pi*alpha)
    xp0 = 1/(alpha*a)
    xpp0 = xp0*(1+alpha)/alpha
    xppp0 = xpp0*(1+2*alpha)/alpha
    spzp1 = (a^alpha)*gamma(alpha)*sin((pi*alpha)/2)/pi
    rp0 = -sp0*xp0 + (2-alpha)*cpxp0 + (alpha-1)*gpxp0
    rpp0 = -sp0*xpp0 + (2-alpha)*cpxpp0 + (alpha-1)*gpxpp0
    rppp0 = -sp0*xppp0 - sppp0*xp0^3 + (2-alpha)*cppp + (alpha-1)*gppp
    rp1 = -spzp1 + (2-alpha)/pi 
    alf2i = (2-alpha)^(1:4)-1
    r = (2-alpha)*(alf2i[1]*s[1,]+alf2i[2]*s[2,]+alf2i[3]*s[3,]+alf2i[4]*s[4,])   
    
    # Setup Q:
    q[1] = 0
    q[2] = rp0
    q[3] = rpp0/2
    q[4] = rppp0/6
    bb = -sum(u*q[2:4]) - sum(r*zn5)
    cc = rp1 - sum(ei*q[2:4]) - 5*sum(r*zn4)
    q[5] = 5*bb - cc
    q[6] = bb - q[4+1]     
    
    # Setup P and PD:
    p[, 1] = q
    for (i in 0:5) {
        for (j in 1:19) {
            p[i+1, j+1] = q[i+1] + cumsum(r*zji[, i+1])[j]
        }     
    }  
    for (i in 1:5) pd[i, ] = i*p[i+1, ]    
    
    # Loop over all datapoints:
    for (I in 1:N) {
        x = X[I]
        xa1 = 1 + a*abs(x)
        xa1a = xa1^(-alpha)
        z = 1 - xa1a
        zp = (alpha*a)*xa1a/xa1
        x1 = ((1-z)^(-1)-1)
        x2 = ((1-z)^(-0.5)-1) / a2
        x1p = 1 / ((1+x1)^(-2))
        x2p = 1 / (2*a2*(1+a2*x2)^(-3))
        j = floor(20 * z)
        j = min(19, j) 
        # RZ:
        A = as.vector(p[(0:5)+1, j+1])
        K = 5
        poly = A[K+1]
        for (j in K:1) poly = poly * z + A[j]
        rz = poly
        # RPZ:
        A = as.vector(pd[(0:4)+1, j+1])
        K = 4
        poly = A[K+1]
        for (j in K:1) poly = poly * z + A[j]
        rpz = poly
        # Cumulated probability function:
        cfun = 0.5 - atan(x1) / pi
        gfun = 1 - pnorm(x2 / sqrt(2))
        probfun = (2-alpha)*cfun + (alpha-1)*gfun + rz
        if (x < 0) probfun = 1 - probfun 
        prob[I] = 1 - probfun
        if (prob[I] < 2.2e-4) prob[I] = ca * abs(x)^(-alpha)
        
        # Probability density function:
        cden = 1 / (pi*(1+x1*x1))
        gden = exp(-x2*x2/4)/(2*sqpi)
        probden = ((2-alpha)*cden*x1p + (alpha-1)*gden*x2p - rpz) * zp
        dens[I] = probden
        if (dens[I] < 6.6e-4) dens[I] = alpha*ca*abs(x)^(-alpha-1)
    }
      
    # Return Value:
    cbind(x = X, p = prob, d = dens)
}  


################################################################################

