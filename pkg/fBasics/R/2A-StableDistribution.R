
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             SYMMETRIC STABLE DISTRIBUTION:
#  dsymstb               Returns density for symmetric stable DF
#  psymstb               Returns probabilities for symmetric stable DF
#  qsymstb               Returns quantiles for symmetric stable DF
#  rsymstb               Returns random variates for symmetric stable DF
#  .symstb               Returns symmetric alpha-stable pdf/cdf 
#  .symstbPoly           Internal Function called from .symstb()
# FUNCTIONS:            STABLE DISTRIBUTION:
#  dstable               Returns density for stable DF
#  pstable               Returns probabilities for stable DF
#  qstable               Returns quantiles for stable DF
#  rstable               Returns random variates for stable DF
#  stableMode            Computes the mode of the stable DF
#  .integrateStable      Integrates internal functions for *stable
# FUNCTION:             STABLE SLIDERS:
#  symstbSlider          Displays symmetric stable distribution function
#  stableSlider          Displays stable distribution function
################################################################################


################################################################################
#  dsymstb               Returns density for symmetric stable DF
#  psymstb               Returns probabilities for symmetric stable DF
#  qsymstb               Returns quantiles for symmetric stable DF
#  rsymstb               Returns random variates for symmetric stable DF
#  .symstb               Internal function 
#  .symstbPoly           Internal Function


dsymstb =
function(x, alpha = 1.8)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density for symmetric stable DF
    
    # FUNCTION:
    
    # Density:
    ans = as.vector(.symstb(x = x, alpha = alpha)[, "d"])
    
    # Attributes:
    attr(ans, "control") = 
        cbind.data.frame(dist = "symstb", alpha = alpha, row.names = "")
      
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


psymstb =
function(q, alpha = 1.8)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns probabilities for symmetric stable DF
    
    # FUNCTION:
    
    # Probability:
    ans = as.vector(.symstb(x = q, alpha = alpha)[, "p"])
    
    # Attributes:
    attr(ans, "control") = 
        cbind.data.frame(dist = "symstb", alpha = alpha, row.names = "")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


qsymstb = 
function(p, alpha)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns quantiles for symmetric stable DF
    
    # FUNCTION:
    
    # Parameter Check:
    if (alpha > +2)  stop("Error: alpha greater than 2")
    if (alpha <= 0)  stop("Error: alpha less or equal 0")
    
    # Special Cases:
    if (alpha == 2) result = qnorm(p = p, mean = 0, sd = sqrt(2))
    if (alpha == 1) result = qcauchy(p = p) 
    
    # Continue:
    if (alpha != 1 && alpha != 2) {
        .froot <<- function(x, alpha, p) {
            psymstb(q = x, alpha = alpha) - p 
        }
        # Calculate:    
        result = rep(NA, times = length(p))
        for (i in 1:length(p)) {
            pp = p[i]
            # xmin = -(1-pp)/pp
            if (pp < 0.5) {
                xmin = qcauchy(pp)
            } else {
                xmin = qnorm(pp, mean = 0, sd = sqrt(2))
            }
            # xmax = pp/(1-pp) 
            if (pp < 0.5) {
                xmax = qnorm(pp, mean = 0, sd = sqrt(2))
            } else {
                xmax = qcauchy(pp)  
            }       
            iteration = NA
            counter = 0
            while (is.na(iteration)) {
                iteration = .unirootNA(f = .froot, interval = c(xmin, xmax), 
                    alpha = alpha, p = pp)
                counter = counter + 1
                xmin = xmin - 2^counter
                xmax = xmax + 2^counter
            }
            result[i] = iteration 
        } 
    }
    
    # Attributes:
    ans = result
    attr(ans, "control") = 
        cbind.data.frame(dist = "symstb", alpha = alpha, row.names = "")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rsymstb = 
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
        rz = .symstbPoly(as.vector(p[(0:5)+1, j+1]), z, 5)
        rpz = .symstbPoly(as.vector(pd[(0:4)+1, j+1]), z, 4)
        
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
 

# ------------------------------------------------------------------------------      

                                              
.symstbPoly = 
function(a, x, k) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function called from .symstb()
    
    # FUNCTION:
    
    # Compute Polynome:
    poly = a[k+1]
    for (j in k:1) poly = poly * x + a[j]
    
    # Return Value:
    poly
}


################################################################################
#  dstable               Returns density for stable DF
#  pstable               Returns probabilities for stable DF
#  qstable               Returns quantiles for stable DF
#  rstable               Returns random variates for stable DF
#  stableMode            Computes stable mode
#  .integrateStable      Integrates internal functions for *stable


dstable = 
function(x, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density for stable DF
    
    # Details:
    #   The function uses the approach of J.P. Nolan for general 
    #   stable distributions. Nolan derived expressions in form 
    #   of integrals based on the charcteristic function for
    #   standardized stable random variables. These integrals
    #   can be numerically evaluated. 
     
    # Arguments:
    #   alpha = index of stability, in the range (0,2]
    #   beta  = skewness, in the range [-1, 1]
    #   gamma = scale, in the range (0, infinity)
    #   delta = location, in the range (-infinity, +infinity)
    #   param = type of parmeterization
    
    # Notes: 
    #   The function doesn't apply for x[i] == 1, this has to be fixed!
    #   For R and SPlus compatibility use integrate()[[1]] instead of
    #       integrate()$value and integrate()$integral.
    #   optimize() works in both R and SPlus.

    # FUNCTION:
    
    # Settings:
    subdivisions = 1000
    tol = .Machine$double.eps
    if (class(version) == "Sversion") {
        subdivisions = 100
        tol = sqrt(tol)
    }
    
    # Parameter Check:
    if (alpha > +2)  stop("Error: alpha greater than 2")
    if (alpha <= 0)  stop("Error: alpha less or equal 0")
    if (beta  < -1)  stop("Error: beta less than -1")
    if (beta  > +1)  stop("Error: beta greater than 1")
    
    # Parameterizations:
    pm = pm[1]
    if (pm == 1) {
        if (alpha == 1) {
            delta = delta + beta*(2/pi)*gamma*log(gamma)
        } else {
            delta = delta + beta*gamma*tan(pi*alpha/2)
        }
    }
    if (pm == 2) {
        delta = delta - alpha^(-1/alpha)*gamma*stableMode(alpha, beta)
        gamma = alpha^(-1/alpha) * gamma
    }
    
    # Special Cases:
    if (alpha == 2) {
        result = dnorm(x, mean = 0, sd = sqrt(2))
    }
    if (alpha == 1 & beta == 0) {
        result = dcauchy(x) 
    }
    
    # Shift and Scale:
    x = (x - delta) / gamma
    
    # General Case 0 < alpha < 2  and  -1 <= beta <= 1 :
    if (abs(alpha-1) < 1 & alpha != 1 & abs(beta) <= 1) {
        # Function to Integrate:
        g1 <<- 
        function(x, xarg, alpha, beta) {
            varzeta = -beta * tan(pi*alpha/2)
                theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
                v = (cos(alpha*theta0))^(1/(alpha-1)) *
                    (cos(x)/sin(alpha*(theta0+x)))^(alpha/(alpha-1)) *
                    (cos(alpha*theta0+(alpha-1)*x)/cos(x))
                g = (xarg-varzeta)^(alpha/(alpha-1)) * v
                gval = g * exp(-g) 
            gval
        }
        # Integration:  
        fct1 <<-
        function(xarg, alpha, beta, tol, subdivisions) { 
            varzeta = -beta * tan(pi*alpha/2)
                theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
            theta2 = optimize(f = g1, lower = -theta0, upper = pi/2, 
                maximum = TRUE, tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)$maximum
            c2 = ( alpha / (pi*abs(alpha-1)*(xarg-varzeta)) ) 
            result1 = .integrateStable(f = g1, lower = -theta0, 
                upper = theta2, subdivisions = subdivisions, 
                rel.tol = tol, abs.tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)[[1]]
            result2 = .integrateStable(f = g1, lower = theta2, 
                upper = pi/2, subdivisions = subdivisions, 
                rel.tol = tol, abs.tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)[[1]]
            c2*(result1+result2) 
        }
        # Loop over all x values:
        result = NULL
        varzeta = -beta * tan(pi*alpha/2) 
        for (z in x) { 
          # if (z == varzeta) Modified D.W.
            if (abs(z - varzeta) < 2 * .Machine$double.eps) {
                theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
                result = c(result, gamma(1+1/alpha)*cos(theta0) /
                    (pi*(1+varzeta^2)^(1/(2*alpha)))) 
            } else {
                if (z > varzeta) { 
                    result = c(result, fct1(xarg = z, alpha = alpha, 
                        beta = beta, tol = tol, subdivisions = subdivisions)) 
                }
                if (z < varzeta) {
                    result = c(result, fct1(xarg = -z, alpha = alpha, 
                        beta = -beta, tol = tol, subdivisions = subdivisions))
                }
            }
        }
    }  
        
    # General Case 0 < alpha < 2  and  -1 <= beta <= 1 :
    if (alpha == 1 & abs(beta) <= 1 & beta != 0) {
        # Function to Integrate:
        g2 <<- 
        function(x, xarg, alpha, beta) {
            # x is a non-sorted vector!
                v = (2/pi) * ((pi/2+beta*x) / cos(x)) *
                exp((1/beta)*(pi/2+beta*x)*tan(x))
                g = exp( -pi*xarg/(2*beta) ) * v
            gval = g * exp(-g) 
            # replace NA at pi/2
            for (i in 1:length(gval)) if(is.na(gval[i])) gval[i] = 0
            gval 
        }
        # Integration:  
        fct2 <<-
        function(xarg, alpha, beta, tol, subdivisions) { 
            theta2 = optimize(f = g2, lower = -pi/2, upper = pi/2, 
                maximum = TRUE, tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)$maximum
            c2 = 1 / (2*abs(beta)) 
            result1 = .integrateStable(f = g2, lower = -pi/2, 
                upper = theta2, subdivisions = subdivisions, 
                rel.tol = tol, abs.tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)[[1]]
            result2 = .integrateStable(f = g2, lower = theta2, 
                upper = pi/2, subdivisions = subdivisions, 
                rel.tol = tol, abs.tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)[[1]]
            c2*(result1+result2) 
        }
        # Loop over all x values:
        result = NULL
        for (z in x) {
            if (z >= 0) {
                result = c(result, fct2(xarg = z, alpha = alpha, 
                    beta = beta, tol = tol, subdivisions = subdivisions))
            } else {
                result = c(result, fct2(xarg = -z, alpha = alpha, 
                    beta = -beta, tol = tol, subdivisions = subdivisions)) 
            }
        }
    }
    
    # Result:
    ans = result/gamma
    
    # Attributes:
    attr(ans, "control") = 
        cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
            gamma = gamma, delta = delta, pm = pm, row.names = "")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


pstable = 
function(q, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns probability for stable DF
    
    # FUNCTION:
    
    # Settings:
    subdivisions = 1000  
    tol = .Machine$double.eps
    if (class(version) == "Sversion") {
        subdivisions = 100
        tol = sqrt(tol)
    }   
    x = q
    
    # Parameter Check:
    if (alpha > +2)  stop("Error: alpha greater than 2")
    if (alpha <= 0)  stop("Error: alpha less or equal 0")
    if (beta  < -1)  stop("Error: beta less than -1")
    if (beta  > +1)  stop("Error: beta greater than 1")
    
    # Parameterizations:
    pm = pm[1]
    if (pm == 1) {
        if (alpha== 1) {
            delta = delta + beta*(2/pi)*gamma*log(gamma)
        } else {
            delta = delta + beta*gamma*tan(pi*alpha/2)
        }
    }
    if (pm == 2) {
        delta = delta - alpha^(-1/alpha)*gamma*stableMode(alpha, beta)
        gamma = alpha^(-1/alpha) * gamma
    }
    
    # Special Cases:
    if (alpha == 2)  result = pnorm(x, mean = 0, sd = sqrt(2))
    if (alpha == 1 & beta == 0) result = pcauchy(x) 

    # Shift and Scale:
    x = (x - delta) / gamma

    # General Case 0 < alpha < 2  and  -1 <= beta <= 1 :
    if (abs(alpha-1) < 1 & alpha !=1 & abs(beta) <= 1) {
        # Function to Integrate:
        G1 <<- 
        function(x, xarg, alpha, beta) {
            varzeta = -beta * tan(pi*alpha/2)
                theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
                v = (cos(alpha*theta0))^(1/(alpha-1)) *
                    (cos(x)/sin(alpha*(theta0+x)))^(alpha/(alpha-1)) *
                    cos(alpha*theta0+(alpha-1)*x)/cos(x)
                g = (xarg-varzeta)^(alpha/(alpha-1)) * v
                gval = exp(-g)
            gval
        }
        # Integration:  
        FCT1 <<-
        function(xarg, alpha, beta, tol, subdivisions) { 
            varzeta = -beta * tan(pi*alpha/2)
                theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
            theta2 = optimize(f = G1, lower = -theta0, upper = pi/2, 
                maximum = TRUE, tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)$maximum
            if (alpha < 1) c1 = (1/pi)*(pi/2-theta0)
            if (alpha > 1) c1 = 1
            c3 = sign(1-alpha)/pi
            result1 = .integrateStable(f = G1, lower = -theta0, 
                upper = theta2, subdivisions = subdivisions, 
                rel.tol = tol, abs.tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)[[1]]
            result2 = .integrateStable(f = G1, lower = theta2, 
                upper = pi/2, subdivisions = subdivisions, 
                rel.tol = tol, abs.tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)[[1]]
            c1 + c3*(result1+result2) 
        }
        # Loop over all x values:
        result = rep(0, times = length(x))  
        for ( i in 1:length(result) ) { 
        varzeta = -beta * tan(pi*alpha/2)
            if (abs(x[i] - varzeta) < 2 * .Machine$double.eps) { 
                theta0 = (1/alpha) * atan( beta * tan(pi*alpha/2))
                result[i] = (1/pi)*(pi/2-theta0)
            } else { 
                if (x[i] > varzeta) result[i] = 
                    FCT1(xarg = x[i], alpha = alpha, beta = beta, 
                        tol = tol, subdivisions = subdivisions)
                if (x[i] < varzeta) result[i] = 
                    1 - FCT1(xarg = -x[i], alpha = alpha, beta = -beta, 
                        tol = tol, subdivisions = subdivisions)
            }
         }
    }
                    
    # General alpha == 1 and  0 < |beta| <= 1 :
    if (alpha == 1 & abs(beta) <= 1 & beta != 0) {
        # Function to Integrate:
        G2 <<- 
        function(x, xarg, alpha, beta) {
            # x is a non-sorted vector!
                v = (2/pi) * ((pi/2+beta*x) / cos(x)) *
                exp((1/beta)*(pi/2+beta*x)*tan(x))
                g = exp( -pi*xarg/(2*beta) ) * v
            gval = exp(-g) 
            # replace NA at pi/2
            for (i in 1:length(gval)) if(is.na(gval[i])) gval[i] = 0
            gval 
        }
        # Integration:  
        FCT2 <<-
        function(xarg, alpha, beta, tol, subdivisions) { 
            theta2 = optimize(f = G2, lower = -pi/2, upper = pi/2, 
                maximum = TRUE, tol = tol, xarg = xarg, 
                alpha=alpha, beta = beta)$maximum
            c3 = 1/pi
            result1 = .integrateStable(f = G2, lower = -pi/2, 
                upper = theta2, subdivisions = subdivisions, 
                rel.tol = tol, abs.tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)[[1]]
            result2 = .integrateStable(f = G2, lower = theta2, 
                upper = pi/2, subdivisions = subdivisions, 
                rel.tol = tol, abs.tol = tol, xarg = xarg, 
                alpha = alpha, beta = beta)[[1]]
            c3*(result1+result2) 
        }
        # Loop over all x values:
        result = rep(0, times = length(x))  
        for ( i in 1:length(result) ) { 
            if (beta >= 0) {
                result[i] = FCT2(xarg = x[i], alpha = alpha, 
                    beta = beta, tol = tol, subdivisions = subdivisions)
            } else {
                result[i] = 1 - FCT2(xarg = -x[i], alpha = alpha, 
                    beta = -beta, tol = tol, subdivisions = subdivisions)
            }
        }
    }

    # Attributes:
    ans = result
    attr(ans, "control") = 
        cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
            gamma = gamma, delta = delta, pm = pm, row.names = "")
            
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


qstable = 
function(p, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns quantiles for stable DF
    
    # FUNCTION:
    
    # Settings:
    subdivisions = 1000
    tol = .Machine$double.eps
    if (class(version) == "Sversion") {
        subdivisions = 100
        tol = sqrt(tol)
    }   

    # Parameter Check:
    if (alpha > +2)  stop("Error: alpha greater than 2")
    if (alpha <= 0)  stop("Error: alpha less or equal 0")
    if (beta  < -1)  stop("Error: beta less than -1")
    if (beta  > +1)  stop("Error: beta greater than 1")
    
    # Parameterizations:
    pm = pm[1]
    if (pm == 1) {
        if (alpha== 1) {
            delta = delta + beta*(2/pi)*gamma*log(gamma)
        } else {
            delta = delta + beta*gamma*tan(pi*alpha/2)
        }
    }
    if (pm == 2) {
        delta = delta - alpha^(-1/alpha)*gamma*stableMode(alpha, beta)
        gamma = alpha^(-1/alpha) * gamma
    }
    
    # Special Cases:
    if (alpha == 2)  result = qnorm(p, mean = 0, sd = sqrt(2))
    if (alpha == 1 & beta == 0) result = qcauchy(p) 
    
    # Range 0 < alpha < 2:
    if (abs(alpha-1) < 1) {
        .froot <<- function(x, alpha, beta, subdivisions, p) {
            pstable(q = x, alpha = alpha, beta = beta, pm = 0) - p 
        }
        # Calculate:
        result = rep(NA, times = length(p))
        for (i in 1:length(p)) {
            pp = p[i]
            if (beta < 0) {
                xmin = -(1-pp)/pp
                # xmax = pp/(1-pp)
                if (pp < 0.5) {
                    xmax = qnorm(pp, mean = 0, sd = sqrt(2))
                } else {
                    xmax = qcauchy(pp)
                }
            }
            if (beta > 0 ) {
                # xmin = -(1-pp)/pp
                if (pp < 0.5) {
                    xmin = qcauchy(pp)
                } else {
                    xmin = qnorm(pp, mean = 0, sd = sqrt(2))
                }
                xmax = pp/(1-pp)
            }
            if (beta == 0 ) {
                # xmin = -(1-pp)/pp
                if (pp < 0.5) {
                    xmin = qcauchy(pp)
                } else {
                    xmin = qnorm(pp, mean = 0, sd = sqrt(2))
                }
                # xmax = pp/(1-pp) 
                if (pp < 0.5) {
                    xmax = qnorm(pp, mean = 0, sd = sqrt(2))
                } else {
                    xmax = qcauchy(pp)
                }
            }
            iteration = NA
            counter = 0
            while (is.na(iteration)) {
                iteration = .unirootNA(f = .froot, interval = c(xmin, xmax), 
                    alpha = alpha, beta = beta,  p = pp, subdivisions =
                    subdivisions)
                counter = counter + 1
                xmin = xmin-2^counter
                xmax = xmax+2^counter
            }
            result[i] = iteration
        }
    }
    
    # Result:
    ans = result * gamma + delta
    
    # Attributes:
    attr(ans, "control") = 
        cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
            gamma = gamma, delta = delta, pm = pm, row.names = "")
            
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rstable = 
function(n, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns random variates for stable DF

    # FUNCTION:
    
    # Parameter Check:
    if (alpha > +2)  stop("Error: alpha greater than 2")
    if (alpha <= 0)  stop("Error: alpha less or equal 0")
    if (beta  < -1)  stop("Error: beta less than -1")
    if (beta  > +1)  stop("Error: beta greater than 1")
    
    # Parameterizations:
    pm = pm[1]
    if (pm == 1) {
        if (alpha== 1) {
            delta = delta + beta*(2/pi)*gamma*log(gamma)
        } else {
            delta = delta + beta*gamma*tan(pi*alpha/2)
        }
    }
    if (pm == 2) {
        delta = delta - alpha^(-1/alpha)*gamma*stableMode(alpha, beta)
        gamma = alpha^(-1/alpha) * gamma
    }
    
    # Calculate uniform and exponential distributed random numbers:
    theta = pi * (runif(n)-1/2)
    w = -log(runif(n))
    
    # If alpha is equal 1 then:
    if (alpha == 1 & beta == 0) {
        result = rcauchy(n)        
        # Otherwise, if alpha is different from 1:
    } else { 
        c = (1+(beta*tan(pi*alpha/2))^2)^(1/(2*alpha))
        theta0 = (1/alpha)*atan(beta*tan(pi*alpha/2))
        result = ( c*sin(alpha*(theta+theta0))/
            (cos(theta))^(1/alpha) ) *
            (cos(theta-alpha*(theta+theta0))/w)^((1-alpha)/alpha) 
        # Use Parametrization 0:
        result = result - beta * tan(alpha*pi/2)
    }
    
    # Result:
    ans = result * gamma + delta
    
    # Attributes:
    attr(ans, "control") = 
        cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
            gamma = gamma, delta = delta, pm = pm, row.names = "")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


stableMode = 
function(alpha, beta) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the mode of the stable DF
    
    # Notes:
    #   # Test for values close to beta = 1
    #   alpha = seq(0, 2, by = 0.1)
    #   ans = NULL
    #   for ( i in 1:length(alpha) ) {
    #     ans = rbind(ans, c(alpha[i], 
    #       stableMode(alpha = alpha[i], beta = 0.99 ),
    #       stableMode(alpha = alpha[i], beta = 0.99999 ),
    #       stableMode(alpha = alpha[i], beta = 0.99999999 ),
    #       stableMode(alpha = alpha[i], beta = 0.99999999999 ) ) ) }
    #   ans
    #
    #   alpha          0.99       0.99999    0.99999999 0.99999999999
    #   0.0    0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
    #   0.2   -3.214142e-01 -3.246759e-01 -3.246787e-01 -3.246788e-01
    #   0.4   -6.105318e-01 -6.158562e-01 -6.158616e-01 -6.158616e-01
    #   0.6   -6.550106e-01 -6.594746e-01 -6.594790e-01 -6.594790e-01
    #   0.8   -5.558811e-01 -5.590032e-01 -5.590063e-01 -5.590063e-01
    #   1.0   -4.271033e-01 -4.293078e-01 -4.293099e-01 -4.293099e-01
    #   1.2   -3.074015e-01 -3.090820e-01 -3.090804e-01 -3.090804e-01
    #   1.4   -2.050956e-01 -2.063979e-01 -2.063951e-01 -2.063951e-01
    #   1.6   -1.199623e-01 -1.208875e-01 -1.208853e-01 -1.208853e-01
    #   1.8   -5.098617e-02 -5.145758e-02 -5.145639e-02 -5.145639e-02
    #   2.0   -7.487432e-05 -7.487432e-05 -7.487432e-05 -7.487432e-05

    # FUNCTION:
    
    # Stable Mode:
    if (beta > 0.99999999999) beta = 0.99999999999
    if (beta == 0) {
        ans = 0 
    } else {
        if (alpha == 0) {
            ans = 0
        } else {
            ans = optimize(f = dstable, interval = c(-0.7, 0), 
                maximum = TRUE, alpha = alpha, beta = beta)$maximum
        }
    }
    
    # Attributes:
    attr(ans, "control") = 
        cbind.data.frame(dist = "stable", alpha = alpha, beta = beta,
        row.names = "")
            
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


.integrateStable = 
function (f, lower, upper, subdivisions, rel.tol, abs.tol, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Integrate:
    if (class(version) != "Sversion") {
        # R:
        f = match.fun(f)
        ff = function(x) f(x, ...)
        wk = .External("call_dqags", ff, 
            rho = environment(), as.double(lower), 
            as.double(upper), as.double(abs.tol), 
            as.double(rel.tol), limit = as.integer(subdivisions), 
            PACKAGE = "base")
        ans = wk[c("value", "abs.error", "subdivisions")] 
    } else {
        # SPlus:
        ans = integrate(f, lower, upper, subdivisions, rel.tol, abs.tol, ...) 
    }
    
    # Return Value:
    ans

}



################################################################################
#  symstbSlider          Displays symmetric stable distribution function
#  stableSlider          Displays stable distribution function


symstbSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # Description
    #   Displays the symmetric stable distribution

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N     = .sliderMenu(no = 1)
        alpha = .sliderMenu(no = 2)
        
        # Compute Data:        
        xmin = round(qsymstb(0.01, alpha), digits = 2)
        xmax = round(qsymstb(0.99, alpha), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dsymstb(s, alpha)
        y2 = psymstb(s, alpha)
        main1 = paste("Symmetric Stable Density\n", 
            "alpha = ", as.character(alpha))
        main2 = paste("Symmetric Stable Probability\n",
            "xmin [0.01%] = ", as.character(xmin), " | ",
            "xmax [0.99%] = ", as.character(xmax))       
        
        # Frame:
        par(mfrow = c(2, 1), cex = 0.7)
        
        # Density:
        plot(s, y1, type = "l", xlim = c(xmin, xmax), col = "steelblue")
        abline (h = 0, lty = 3)
        title(main = main1)     
        
        # Probability:
        plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
            col = "steelblue" )
        abline (h = 0, lty = 3)
        title(main = main2) 
        
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(  "N", "alpha"),
       minima =      c(   50,   0.10),
       maxima =      c( 1000,   2.00),
       resolutions = c(   50,   0.10),
       starts =      c(   50,   1.75))
}


# ------------------------------------------------------------------------------


stableSlider =  
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays the stable distribution

    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N     = .sliderMenu(no = 1)
        alpha = .sliderMenu(no = 2)
        beta  = .sliderMenu(no = 3)
        gamma = .sliderMenu(no = 4)
        delta = .sliderMenu(no = 5)
        pm    = .sliderMenu(no = 6)
         
        # Compute Data:  
        xmin = round(qstable(0.01, alpha, beta, gamma, delta, pm), digits = 2)
        xmax = round(qstable(0.99, alpha, beta, gamma, delta, pm), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dstable(s, alpha, beta, gamma, delta, pm)
        y2 = pstable(s, alpha, beta, gamma, delta, pm)
        main1 = paste("Stable Density\n", 
            "alpha = ", as.character(alpha), " | ",
            "beta = ", as.character(beta), " | ",
            "gamma = ", as.character(gamma), " | ",
            "delta = ", as.character(delta))
        main2 = paste("Stable Probability\n",
            "xmin 0.01% = ", as.character(xmin), " | ",
            "xmax 0.99% = ", as.character(xmax), " | ",
            "pm = ", as.character(pm))        
        
        # Frame:
        par(mfrow = c(2, 1), cex = 0.7)   
        
        # Density:
        plot(s, y1, type = "l", xlim = c(xmin, xmax), col = "steelblue")
        abline (h = 0, lty = 3)
        title(main = main1)       
        
        # Probability:
        plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
            col = "steelblue" )
        abline(h = 0.0, lty = 3)
        abline(h = 1.0, lty = 3)
        abline(h = 0.5, lty = 3)
        abline(v = delta, lty = 3, col = "red")
        title(main = main2)      
        
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(  "N", "alpha", "beta", "gamma", "delta", "pm"),
       minima =      c(   10,    0.00,  -1.00,    0.00,    -5.0,    0),
       maxima =      c( 1000,    2.00,  +1.00,    5.00,    +5.0,    2),
       resolutions = c(   50,    0.20,   0.20,    1.00,     1.0,    1),
       starts =      c(   50,    1.80,   0.00,    1.00,     0.0,    0))
}


################################################################################

