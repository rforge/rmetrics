
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
# FUNCTIONS:            STABLE DISTRIBUTION:
#  stableMode            Computes stable mode
#  dstable               Returns density for stable DF
#  pstable               Returns probabilities for stable DF
#  qstable               Returns quantiles for stable DF
#  rstable               Returns random variates for stable DF
# FUNCTION:             STABLE SLIDERS:
#  symstbSlider          Displays symmetric stable distribution function
#  stableSlider          Displays stable distribution function
################################################################################


################################################################################
#  dsymstb               Returns density for symmetric stable DF
#  psymstb               Returns probabilities for symmetric stable DF
#  qsymstb               Returns quantiles for symmetric stable DF
#  rsymstb               Returns random variates for symmetric stable DF


dsymstb = 
function (x, alpha)
{   # # A function implemented by Diethelm Wuertz
  
    # Description:
    #   Return symmetric alpha-stable pdf
    
    # Note: 
    #   symstb - returns symmetric alpha-stable pdf/cdf. The function  
    #   implements J.H. McCulloch's Fortran program for symmetric 
    #   distributions. Mc Cullochs approach has a density precision of 
    #   0.000066 and a distribution precision of 0.000022 for alpha in  
    #   the range [0.84, 2.00]. We have added only first order tail 
    #   approximation to calculate the tail density and probability.
    #   This has still to be improved!

    # Changes:
    #
    
    # FUNCTION:
    
    # Density:
    ans = .Fortran("symstb",
        as.double(x),
        as.double(1:length(x)),
        as.double(1:length(x)),
        as.integer(length(x)),
        as.double(alpha),
        PACKAGE = "fBasics")[[3]]
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


psymstb = 
function (q, alpha)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Return symmetric alpha-stable cdf

    # Notes: 
    #   symstb:
    #   Return symmetric alpha-stable pdf/cdf. The function
    #   implements J.H. McCulloch's Fortran program for symmetric
    #   distributions.
    #   Mc Cullochs approach has a density precision of 0.000066
    #   and a distribution precision of 0.000022 for alpha in the 
    #   range [0.84, 2.00]. We have added only first order tail 
    #   approximation to calculate the tail density and probability.
    #   This has still to be improved!
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Return Value:
    ans = .Fortran("symstb",
        as.double(q),
        as.double(1:length(q)),
        as.double(1:length(q)),
        as.integer(length(q)),
        as.double(alpha),
        PACKAGE = "fBasics")[[2]]
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


qsymstb = 
function(p, alpha)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
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
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rsymstb = 
function(n, alpha) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Return random deviates from the stable family 
    #   of probability distributions. The results of 
    #   Chambers, Mallows, and Stuck is used.

    # Changes:
    #
    
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
    attr(result, "control") = c(dist = "symstb", alpha = as.character(alpha))
    
    # Return Value:
    result
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
    #   Return alpha-stable density function (pdf) in form
    #   of parmeterization 1. 
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

    # Changes:
    #
    
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
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


pstable = 
function(q, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
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

    # Return Value:
    result
}


# ------------------------------------------------------------------------------


qstable = 
function(p, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
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
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rstable = 
function(n, alpha, beta, gamma = 1, delta = 0, pm = c(0, 1, 2))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Return random deviates from the stable family 
    #   of probability distributions.

    # Changes:
    #
    
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
    
    # Add Attribute:
    attr(ans, "control") = c(dist = "stable", alpha = as.character(alpha),
        beta = as.character(beta), gamma = as.character(gamma),
        delta = as.character(delta), pm = as.character(pm))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


stableMode = 
function(alpha, beta) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the mode of the stable distribution function
    
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

    # Changes:
    #
    
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
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


.integrateStable = 
function (f, lower, upper, subdivisions, rel.tol, abs.tol, ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
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
    
    # Changes:
    #
    
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

    # Changes:
    #
    
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

