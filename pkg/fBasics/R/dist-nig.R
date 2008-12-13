
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:
#  dnig                  Returns density for inverse Gaussian DF
#  pnig                  Returns probability for for inverse Gaussian DF
#  qnig                  Returns quantiles for for inverse Gaussian DF 
#  rnig                  Returns random variates for inverse Gaussian DF
################################################################################


dnig <- 
    function(x, alpha = 1, beta = 0, delta = 1, mu = 0, log = FALSE)
{   
    # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Density:
    dgh(x = x, alpha = alpha, beta = beta, delta = delta, mu = mu, 
        lambda = -0.5, log = log)
}


# ------------------------------------------------------------------------------


pnig <- 
    function(q, alpha = 1, beta = 0, delta = 1, mu = 0)
{   
    # A function implemented by Diethelm Wuertz

    # Function:
    
    # Probability:
    pgh(q = q, alpha = alpha, beta = beta, delta = delta, mu = mu, 
        lambda = -0.5)
}


# ------------------------------------------------------------------------------


qnig <- 
    function(p, alpha = 1, beta = 0, delta = 1, mu = 0)
{   
    # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Quantiles
    qgh(p = p, alpha = alpha, beta = beta, delta = delta, mu = mu, 
        lambda = -0.5)
}


# ------------------------------------------------------------------------------


rnig <- 
    function(n, alpha = 1, beta = 0, delta = 1, mu = 0)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Return normal inverse Gaussian distributed random variates
    
    # Arguments:
    #   n - number of deviates to be generated
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION: 
    
    # Settings:
    gamma = sqrt(alpha*alpha - beta*beta)
    
    # GAMMA:
    if (gamma == 0) {
        # GAMMA = 0:
        V = rnorm(n)^2
        Z = delta*delta / V
        X = sqrt(Z)*rnorm(n) 
    } else {    
        # GAMMA > 0:
        U = runif(n)
        V = rnorm(n)^2
        # FIXED ...
        z1 <- function(v, delta, gamma) {
            delta/gamma + v/(2*gamma^2) - sqrt(v*delta/(gamma^3) + 
            (v/(2*gamma^2))^2 ) 
        }
        z2 <- function(v, delta, gamma) {
            (delta/gamma)^2 / z1(v = v, delta = delta, gamma = gamma)
        }
        pz1 <- function(v, delta, gamma) {
            delta / (delta + gamma * z1(v = v, delta = delta, gamma = gamma) ) 
        }
        s = (1-sign(U-pz1(v = V, delta = delta, gamma = gamma)))/2
        Z = z1(v = V, delta = delta, gamma = gamma)*s + z2(v = V, delta = 
            delta, gamma = gamma)*(1-s)
        X = mu + beta*Z + sqrt(Z)*rnorm(n) 
    }
    
    # Attributes:
    attr(X, "control") = c(dist = "nig", alpha = alpha, beta = beta, 
        delta = delta, mu = mu)
    
    
    # Return Value:
    X
}


################################################################################

