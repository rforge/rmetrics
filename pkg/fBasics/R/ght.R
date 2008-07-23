
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
#  dght                  Hyperbolic Distribution - Skew Symmetric Student-t
################################################################################


dght <-
function(x, beta = 1e-6, delta = 1, mu = 0, nu = 10, log = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns density of generalized hyperbolic Student-t

    # Arguments:

    # Details:
    #   Hyperbolic Distribution - Skew Symmaetric Student-t:
    #   dght(x, beta = 0.1, delta = 1, mu = 0, nu = 10, log = FALSE)

    # FUNCTION:

    # Density:
    D = sqrt( delta^2 + (x-mu)^2 )
    A1 = ((1-nu)/2) * log(2)
    A2 = nu * log(delta)
    A3 = ((nu+1)/2) * log(abs(beta))
    A4 = log(besselK(abs(beta)*D, (nu+1)/2, expon.scaled = TRUE)) - abs(beta)*D
    A5 = beta*(x-mu)
    B1 = lgamma(nu/2)
    B2 = log(sqrt(pi))
    B3 = ((nu+1)/2) * log(D)

    # Log:
    ans = (A1 + A2 + A3 + A4 + A5) - (B1 + B2 + B3)
    if (!log) ans = exp(ans)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


pght <-
    function(q, beta = 1e-6, delta = 1, mu = 0, nu = 10, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns probabilities of generalized hyperbolic Student-t

    # Arguments:

    # FUNCTION:

    # Cumulative Probability:
    ans = NULL
    for (Q in q) {
        Integral = integrate(dght, -Inf, Q, stop.on.error = FALSE,
            beta = beta, delta = delta, mu = mu, nu = nu, ...)
        ans = c(ans, as.numeric(unlist(Integral)[1]) )
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


qght <-
    function(p, beta = 1e-6, delta = 1, mu = 0, nu = 10, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns quanbtiles of generalized hyperbolic Student-t

    # Arguments:

    # FUNCTION:

    # Internal Functions:
    .froot <-
    function(x, beta = beta, delta = delta, mu = mu, nu = nu, p)
    {
        phyp(q = x, beta = beta, delta = delta, mu = mu, nu = nu,) - p
    }

    # Loop over all p's:
    result = NULL
    for (pp in p) {
        lower = -1
        upper = +1
        counter = 0
        iteration = NA
        while (is.na(iteration)) {
            iteration = fUtilities:::.unirootNA(f = .froot, interval = c(lower, upper),
                beta = beta, delta = delta, mu = mu, nu = nu,, p = pp, ...)
            counter = counter + 1
            lower = lower-2^counter
            upper = upper+2^counter
        }
        result = c(result, iteration)
    }

    # Return Value:
    ans = result + mu
    ans
}


# ------------------------------------------------------------------------------


.rght =
function(p, beta = 1e-6, delta = 1, mu = 0, nu = 10, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns random Variates of generalized hyperbolic Student-t

    # Arguments:

    # FUNCTION:

    # Random Variates:
    # ... to do

    # Return Value:
    NA
}


################################################################################
