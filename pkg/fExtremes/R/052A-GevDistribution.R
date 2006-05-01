
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             GEV DISTRIBUTION FAMILY: [USE FROM EVD]
#  devd                  Density for the GEV Distribution 
#   pevd                  Probability for the GEV Distribution
#   qevd                  Quantiles for the GEV Distribution
#   revd                  Random variates for the GEV Distribution
# FUNCTION:             GEV DISTRIBUTION FAMILY: [USE FROM EVIS]
#  dgev                  Density for the GEV Distribution 
#   pgev                  Probability for the GEV Distribution
#   qgev                  Quantiles for the GEV Distribution
#   rgev                  Random variates for the GEV Distribution
# FUNCTION:             MOMENTS:
#  gevMoments            Computes true statistics for GEV distribution
################################################################################


devd = 
function(x, location = 0, scale = 1, shape = 0, log = FALSE) 
{   # A modified copy from contributed R package evd

    # FUNCTION:
    
    # Check:
    stopifnot(min(scale) > 0) 
    stopifnot(length(shape) == 1)
    
    # Density:
    x = (x - location) / scale
    if (shape == 0) {
        d = log(1/scale) - x - exp(-x)
    } else {
        nn = length(x)
        xx = 1 + shape * x
        xxpos = xx[xx > 0 | is.na(xx)]
        scale = rep(scale, length.out = nn)[xx > 0 | is.na(xx)]
        d = numeric(nn)
        d[xx > 0 | is.na(xx)] = log(1/scale) - xxpos^(-1/shape) - 
            (1/shape + 1) * log(xxpos)
        d[xx <= 0 & !is.na(xx)] = -Inf
    }
    
    # Log:
    if (!log) {
        d = exp(d)
    }
    
    # Add Attribute:
    attr(d, "control") = data.frame(location = location[1], scale = scale[1], 
        shape = shape[1], log = log, row.names = "")
        
    
    # Return Value:
    d
}


# ------------------------------------------------------------------------------


pevd =
function(q, location = 0, scale = 1, shape = 0, lower.tail = TRUE) 
{   # A modified copy from contributed R package evd

    # FUNCTION:
    
    # Check:
    stopifnot(min(scale) > 0) 
    stopifnot(length(shape) == 1)
    
    # Probabilities:
    q = (q - location)/scale
    if (shape == 0) {
        p = exp(-exp(-q))
    } else {
        p = exp(-pmax(1 + shape * q, 0)^(-1/shape))
    }
    
    # Lower Tail:
    if (!lower.tail) {
        p = 1 - p
    }
    
    # Add Attribute:
    attr(p, "control") = data.frame(location = location[1], scale = scale[1], 
        shape = shape[1], lower.tail = lower.tail, row.names = "")
    
    # Return Value:
    p
}


# ------------------------------------------------------------------------------


qevd = 
function(p, location = 0, scale = 1, shape = 0, lower.tail = TRUE) 
{   # A modified copy from contributed R package evd

    # FUNCTION:
    
    # Check:
    stopifnot(min(scale) > 0) 
    stopifnot(length(shape) == 1)
    stopifnot(min(p, na.rm = TRUE) >= 0)
    stopifnot(max(p, na.rm = TRUE) <= 1) 
    
    # Quantiles:
    if (!lower.tail) p = 1 - p   
    if (shape == 0) {
        q = location - scale * log(-log(p))
    } else {
        q = location + scale * ((-log(p))^(-shape) - 1)/shape
    }
    
    # Add Attribute:
    attr(q, "control") = data.frame(location = location[1], scale = scale[1], 
        shape = shape[1], lower.tail, row.names = "")
           
    # Return Value:
    q
}


# ------------------------------------------------------------------------------


revd =
function(n, location = 0, scale = 1, shape = 0) 
{   # A modified copy from contributed R package evd

    # FUNCTION:
    
    # Check:
    stopifnot(min(scale) > 0) 
    stopifnot(length(shape) == 1)
    
    # Random Variates:   
    if (shape == 0) {
        r = location - scale * log(rexp(n))
    } else {
        r = location + scale * (rexp(n)^(-shape) - 1)/shape
    }
    
    # Add Attribute:
    attr(r, "control") = data.frame(location = location[1], scale = scale[1], 
        shape = shape[1], row.names = "")
        
    # Return Value:
    r
}


################################################################################


dgev =
function(x, xi = 1, mu = 0, sigma = 1, log = FALSE)
{   # A function implemented from evd

    # Description:
    #   GEV Density Function
    #   Note: 1 + xi*(x-mu)/sigma > 0
    #   xi > 0 Frechet
    #   xi = 0 Gumbel
    #   xi < 0 weibl

    # FUNCTION:
    
    # Settings:
    location = mu
    scale = sigma
    shape = xi
    
    # Check:
    stopifnot(min(scale) > 0) 
    stopifnot(length(shape) == 1)
    
    # Density Function:
    x = (x - location)/scale
    if (shape == 0) {
        d = log(1/scale) - x - exp(-x)
    } else {
        nn = length(x)
        xx = 1 + shape * x
        xxpos = xx[xx > 0 | is.na(xx)]
        scale = rep(scale, length.out = nn)[xx > 0 | is.na(xx)]
        d = numeric(nn)
        d[xx > 0 | is.na(xx)] = log(1/scale) - xxpos^(-1/shape) - 
            (1/shape + 1) * log(xxpos)
        d[xx <= 0 & !is.na(xx)] = -Inf
    }
    
    # Log:
    if (!log) {
        d = exp(d)
    }
    
    # Add Attribute:
    attr(d, "control") = data.frame(xi = xi, mu = mu, sigma = sigma,
        log = log, row.names = "")
    
    # Return Value:
    d
}


# ------------------------------------------------------------------------------


pgev =
function(q, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
{   # A function implemented from evd
 
    # Description:
    #   GEV Probability Function
    #   Note: 1 + xi*(x-mu)/sigma > 0
    #   xi > 0 Frechet
    #   xi = 0 Gumbel
    #   xi < 0 Weibull

    # FUNCTION:
    
    # Settings:
    location = mu
    scale = sigma
    shape = xi
    
    # Check:
    stopifnot(min(scale) > 0) 
    stopifnot(length(shape) == 1)
    
    # Probabilities:
    q = (q - location)/scale
    if (shape == 0) {
        p = exp(-exp(-q))
    } else {
        p = exp(-pmax(1 + shape * q, 0)^(-1/shape))
    }
    
    # Lower Tail:
    if (!lower.tail) {
        p = 1 - p
    }
    
    # Add Attribute:
    attr(p, "control") = data.frame(xi = xi, mu = mu, sigma = sigma,
        lower.tail = lower.tail, row.names = "")
    
    # Return Value:
    p
}


# ------------------------------------------------------------------------------


qgev =
function(p, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
{   # A function implemented from evd

    # Description:
    #   GEV Quantile Function
    #   Note: 1 + xi*(x-mu)/sigma > 0
    #   xi > 0 Frechet
    #   xi = 0 Gumbel
    #   xi < 0 Weibull

    # FUNCTION:
    
    # Settings:
    location = mu
    scale = sigma
    shape = xi
    
    # Check:
    stopifnot(min(scale) > 0) 
    stopifnot(length(shape) == 1)
    stopifnot(min(p, na.rm = TRUE) >= 0)
    stopifnot(max(p, na.rm = TRUE) <= 1) 
    
    # Quantiles:
    if (!lower.tail) p = 1 - p
    if (shape == 0) {
        q = location - scale * log(-log(p))
    } else {
        q = location + scale * ((-log(p))^(-shape) - 1)/shape
    }
    
    # Add Attribute:
    attr(q, "control") = data.frame(xi = xi, mu = mu, sigma = sigma,
        lower.tail = lower.tail, row.names = "")
        
    # Return Value:
    q
}


# ------------------------------------------------------------------------------


rgev =
function(n, xi = 1, mu = 0, sigma = 1)
{   # A function implemented from evd

    # Description:
    #   GEV Random Variables
    #   Note: 1 + xi*(x-mu)/sigma > 0
    #   xi > 0 Frechet
    #   xi = 0 Gumbel
    #   xi < 0 Weibull

    # FUNCTION:
    
    # Settings:
    location = mu
    scale = sigma
    shape = xi
    
    # Check:
    stopifnot(min(scale) > 0) 
    stopifnot(length(shape) == 1)
    
    # Random Variates:
    if (shape == 0) {
        r = location - scale * log(rexp(n))
    } else {
        r = location + scale * (rexp(n)^(-shape) - 1)/shape
    }
    
    # Add Attribute:
    attr(r, "control") = data.frame(xi = xi, mu = mu, sigma = sigma,
        row.names = "")
        
    # Return Value:
    r
}


################################################################################


evdMoments =
function(location = 0, scale = 1, shape = 0) 
{   # A function implemented by Diethelm Wuertz
  
    # Description:
    #   Compute true statistics for Generalized Extreme Value distribution
    
    # FUNCTION:
    
    # Moments:
    ans = gevMoments(xi = shape, mu = location, sigma = scale)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


gevMoments = 
function(xi = 0, mu = 0, sigma = 1)
{   # A function implemented by Diethelm Wuertz
  
    # Description:
    #   Compute true statistics for Generalized Extreme Value distribution
    
    # Value:
    #   Returns true mean for xi < 1 and variance for xi < 1/2
    #   of GEV distribution, otherwise NaN is returned

    # FUNCTION: 
    
    # Settings:
    beta = sigma
    
    # MEAN: Returns for x >= 1 NaN:
    g = c(1, 0, NaN)
    xinv = 1/ ( xi + sign(abs(xi)) - 1 )
    
    # For xi = the result is eulers constant
    euler = 0.57721566490153286060651209008240243104    
    xi0 = c(0, mu+beta*euler, 0)
    
    # Supress warning for NaN's from Gamma Function:
    options(warn = -1)
    gevMean = mu + beta * xinv * (gamma(1-xi)-1) * g[sign(xi-1)+2] +
        xi0[(sign(xi)+2)]
    options(warn = 0)
    
    # VAR: Returns for x >= 1 NaN:
    g = c(1, 0, NaN)
    xinv = 1/ ( xi + sign(abs(xi)) - 1 )
    xi0 = c(0, (beta*pi)^2 / 6, 0)
    
    # Supress warning for NaN's from Gamma Function:
    options(warn=-1)
    gevVar = (beta*xinv)^2 * (gamma(1-2*xi) - gamma(1-xi)^2 ) * 
        g[sign(2*xi-1)+2] + xi0[(sign(xi)+2)]
    options(warn = 0)     

    # Return Value:
    list(mean = gevMean, var = gevVar)      
}
    
    
################################################################################

