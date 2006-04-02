
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


# ##############################################################################
# FUNCTION:             GPD DISTRIBUTION FAMILY:
# dgpd                   Density for the Generalized Pareto DF [USE FROM EVIS]
#  pgpd                   Probability for the Generalized Pareto DF
#  qgpd                   Quantiles for the Generalized Pareto DF
#  rgpd                   Random variates for the Generalized Pareto DF
# FUNCTION:             MOMENTS:
#  .gpdMoments           Computes true statistics for GPD distribution
################################################################################


dgpd = 
function(x, xi = 1, mu = 0, beta = 1)
{   # A function written by Diethelm Wuertz

    # FUNCTION:
    
    # Density:
    y = (x - mu)
    if (xi == 0) {
        d = (1-exp(-y/beta))/beta }
    else {
        d = 1/beta * (1 + (xi*y)/beta)^((-1/xi) - 1) }  
    
    d[y < 0] = 0
    if (xi < 0) d[y > (-1/xi)] = 0
    
    # Return Value:
    d
}


# ------------------------------------------------------------------------------


pgpd = 
function(q, xi = 1, mu = 0, beta = 1)
{   # A function written by Diethelm Wuertz

    # FUNCTION:
    
    # Probability:
    y = (q - mu)
    if (xi == 0) {
        p = y/beta + exp(-y/beta) -1 }
    else {
        p = (1 - (1 + (xi*y)/beta)^(-1/xi)) }   
    
    p[y < 0] = 0
    if (xi < 0) p[y > (-1/xi)] = 1
    
    # Return Value:
    p
}


# ------------------------------------------------------------------------------


qgpd = 
function(p, xi = 1, mu = 0, beta = 1)
{   # A function written by Diethelm Wuertz

    # FUNCTION:
    
    # Quantiles:
    if (xi == 0) 
        q = mu - beta*log(1-p)
    else
        q = mu + (beta/xi) * ((1 - p)^( - xi) - 1)
    
    # Return Value:
    q
}


# ------------------------------------------------------------------------------


rgpd = 
function(n, xi = 1, mu = 0, beta = 1)
{   # A function written by Diethelm Wuertz

    # FUNCTION:
    
    # Random variates:
    rvs = mu + (beta/xi) * ((1 - runif(n))^( - xi) - 1)
    
    # Return Value:
    rvs
}


# ------------------------------------------------------------------------------


.gpdMoments = 
function(xi, mu = 0, beta = 1)
{   # A function implemented by Diethelm Wuertz
 
    # Description:
    #   Compute true statistics for Generalized Pareto distribution
    
    # Value:
    #   Returns true mean of Generalized Pareto distribution 
    #   for xi < 1 else NaN
    #   Returns true variance of Generalized Pareto distribution 
    #   for xi < 1 else NaN

    # FUNCTION: 
    
    # MEAN: Rreturns 1 for x <= 0 and -Inf's's else
    a = c(1, NaN, NaN)
    gpdMean = beta/(1-xi)*a[sign(xi-1)+2]
    
    # VAR: Rreturns 1 for x <= 0 and -Inf's's else
    a = c(1, NaN, NaN)
    gpdVar = beta*beta/(1-xi)^2/(1-2*xi) * a[sign(2*xi-1)+2]

    # Return Value:
    list(mean = gevMean, var = gevVar)              
}


################################################################################

