
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
# FUNCTION:             DESCRIPTION:  
#  dgh                   Returns density for generalized hyperbolic DF
#  pgh                   Returns probability for generalized hyperbolic DF
#  qgh                   Returns quantiles for generalized hyperbolic DF
#  rgh                   Returns random variates for generalized hyperbolic DF
#  .rghyp                 Internal functions called by 'rgh'
#  .rgigjd                Internal functions called by 'rghyp'
#  .rgigjd1               Internal functions called by 'rghyp'
# FUNCTION:             DESCRIPTION: 
#  dhyp                  Returns density for hyperbolic DF
#  phyp                  Returns probability for hyperbolic DF
#  qhyp                  Returns quantiles for hyperbolic DF
#  rhyp                  Returns random variates for hyperbolic DF
#  hypMode               Computes the hyperbolic mode
#  .[dpqr]hyp[1234]       Internal functions called by '*hyp'
#  .hyp[1234]Mode         Internal functions called by 'hypMode'
# FUNCTION:             DESCRIPTION:
#  dnig                  Returns density for inverse Gaussian DF
#  pnig                  Returns probability for for inverse Gaussian DF
#  qnig                  Returns quantiles for for inverse Gaussian DF 
#  rnig                  Returns random variates for inverse Gaussian DF
#  nigShapeTriangle      Plots NIG Shape Triangle
# FUNCTION:             DESCRIPTION:
#  dght                  Hyperbolic Distribution - Skew Symmaetric Student-t:
# FUNCTION:             DESCRIPTION:
#  hypSlider             Displays hyperbolic distribution function
#  nigSlider             Displays normal inverse Gausssian distribution function
################################################################################


################################################################################
# FUNCTION:             DESCRIPTION:
#  dgh                   Returns density for generalized hyperbolic DF
#  pgh                   Returns probability for generalized hyperbolic DF
#  qgh                   Returns quantiles for generalized hyperbolic DF
#  rgh                   Returns random variates for generalized hyperbolic DF
#  .rghyp                ... Internal functions for the evaluation
#  .rgigjd                   of random variates for the generalized
#  .rgigjd1                  hyperbolic distribution function ...


dgh = 
function(x, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1, log = FALSE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns density for the generalized hyperbolic distribution

    # FUNCTION:
    
    # Checks:
    if (alpha <= 0) stop("alpha must be greater than zero")  
    if (delta <= 0) stop("delta must be greater than zero")
    if (abs(beta) >= alpha) stop("abs value of beta must be less than alpha")

    # Density:
    arg = delta*sqrt(alpha^2-beta^2)
    a = (lambda/2)*log(alpha^2-beta^2) - (
        log(sqrt(2*pi)) + (lambda-0.5)*log(alpha) + lambda*log(delta) +
        log(besselK(arg, lambda, expon.scaled = TRUE)) - arg )      
    f = ((lambda-0.5)/2)*log(delta^2+(x - mu)^2)
    
    # Use exponential scaled form to prevent from overflows:
    arg = alpha * sqrt(delta^2+(x-mu)^2)
    k = log(besselK(arg, lambda-0.5, expon.scaled = TRUE)) - arg
    e = beta*(x-mu)  
    
    # Put all together:
    ans = a + f + k + e
    if(!log) ans = exp(ans)
    
    # Return Value:  
    ans
}


# ------------------------------------------------------------------------------

    
pgh = 
function(q, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns probability for the generalized hyperbolic distribution
  
    # FUNCTION:
    
    # Checks:
    if (alpha <= 0) stop("alpha must be greater than zero")  
    if (delta <= 0) stop("delta must be greater than zero")
    if (abs(beta) >= alpha) stop("abs value of beta must be less than alpha")
    
    # Probability:
    ans = NULL
    for (Q in q) {
        Integral = integrate(dgh, -Inf, Q, stop.on.error = FALSE, 
            alpha = alpha, beta = beta, delta = delta, mu = mu, 
            lambda = lambda)
        ans = c(ans, as.numeric(unlist(Integral)[1]))
    }
       
    # Return Value: 
    ans
}
    
 
# ------------------------------------------------------------------------------

    
qgh = 
function (p, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns quantiles for the generalized hyperbolic distribution
 
    # FUNCTION:
    
    # Checks:
    if (alpha <= 0) stop("alpha must be greater than zero")  
    if (delta <= 0) stop("delta must be greater than zero")
    if (abs(beta) >= alpha) stop("abs value of beta must be less than alpha")
    
    # Internal Function:
    .froot <- 
    function(x, alpha, beta, delta, mu, lambda, p) 
    {
        pgh(q = x, alpha = alpha, beta = beta, delta = delta, 
            mu = mu, lambda = lambda) - p
    }
    
    # Quantiles:
    result = NULL
    for (pp in p) {
        lower = -1
        upper = +1
        counter = 0
        iteration = NA
        while (is.na(iteration)) {
            iteration = .unirootNA(f = .froot, interval = c(lower, 
                upper), alpha = alpha, beta = beta, delta = delta, 
                mu = mu, lambda = lambda, p = pp)
            counter = counter + 1
            lower = lower - 2^counter
            upper = upper + 2^counter
        }
        result = c(result, iteration)
    }
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rgh = 
function (n, alpha = 1, beta = 0, delta = 1, mu = 0, lambda = 1)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns random variates for the generalized hyperbolic distribution

    # FUNCTION:
    
    # Checks:
    if (alpha <= 0) stop("alpha must be greater than zero")  
    if (delta <= 0) stop("delta must be greater than zero")
    if (abs(beta) >= alpha) stop("abs value of beta must be less than alpha")
    
    # Settings:
    theta = c(lambda, alpha, beta, delta, mu)
    
    # Random Numbers:
    ans = .rghyp(n, theta)
    
    # Attributes:
    attr(ans, "control") = c(dist = "gh", alpha = alpha, beta = beta, 
    delta = delta, mu = mu, lambda = lambda)
    
    # Return Value:
    ans 
}


################################################################################
#  dhyp                  Returns density for hyperbolic DF
#  phyp                  Returns probability for hyperbolic DF
#  qhyp                  Returns quantiles for hyperbolic DF
#  rhyp                  Returns random variates for hyperbolic DF
#  hypMode               Computes the hyperbolic mode
#  .[dpqr]hyp[1234]       Internal functions called by '*hyp'
#  .hyp[1234]Mode         Internal functions called by 'hypMode'


dhyp = 
function(x, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4))
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Hyperbolic Density Function PDF
       
    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter
 
    # FUNCTION:
    
    # Settings:
    pm = pm[1]
    
    # Return Value:
    ans = NA
    if (pm == 1) return(.dhyp1(x, alpha, beta, delta, mu))
    if (pm == 2) return(.dhyp2(x, alpha, beta, delta, mu))
    if (pm == 3) return(.dhyp3(x, alpha, beta, delta, mu))
    if (pm == 4) return(.dhyp4(x, alpha, beta, delta, mu))
}


# ------------------------------------------------------------------------------


phyp = 
function(q, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Return cumulative probability of Hyperbolic PDF
     
    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Settings:
    pm = pm[1]
    
    # Return Value:
    ans = NA
    if (pm == 1) return(.phyp1(q, alpha, beta, delta, mu, ...))
    if (pm == 2) return(.phyp2(q, alpha, beta, delta, mu, ...))
    if (pm == 3) return(.phyp3(q, alpha, beta, delta, mu, ...))
    if (pm == 4) return(.phyp4(q, alpha, beta, delta, mu, ...))
}


# ------------------------------------------------------------------------------


qhyp = 
function(p, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns quantiles of Hyperbolic PDF
    
    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # Note:
    #   This procedure will not run under Splus.
   
    # FUNCTION:
    
    # Settings:
    pm = pm[1]
    
    # Return Value:
    ans = NA
    if (pm == 1) return(.qhyp1(p, alpha, beta, delta, mu, ...))
    if (pm == 2) return(.qhyp2(p, alpha, beta, delta, mu, ...))
    if (pm == 3) return(.qhyp3(p, alpha, beta, delta, mu, ...))
    if (pm == 4) return(.qhyp4(p, alpha, beta, delta, mu, ...))   
}


# ------------------------------------------------------------------------------


rhyp = 
function (n, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4))
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns random deviates of Hyperbolic PDF
    
    # Arguments:
    #   n - number of random deviates to be generated
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter
    
    # Notes:
    #   I have removed my original Fortran program and replaced it by
    #   the dhyperb() function from the HyperbolicDist Package, written
    #   by David Scott, Ai-Wei Lee, Jennifer Tso, Richard Trendall.
    #   License: GPL

    # FUNCTION:
    
    # Settings:
    pm = pm[1]
    
    # Result:
    ans = NA
    if (pm == 1) ans = .rhyp1(n, alpha, beta, delta, mu)
    if (pm == 2) ans = .rhyp2(n, alpha, beta, delta, mu)
    if (pm == 3) ans = .rhyp3(n, alpha, beta, delta, mu)
    if (pm == 4) ans = .rhyp4(n, alpha, beta, delta, mu)
    
    # Attributes:
    attr(ans, "control") = c(dist = "hyp", alpha = alpha, beta = beta, 
    delta = delta, mu = mu)
    
    # Return Value:
    ans
}   


# ------------------------------------------------------------------------------


hypMode =
function(alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4))
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the mode of the Hyperbolic PDF
  
    # FUNCTION:
    
    # Settings:
    pm = pm[1]
    
    # Return Value:
    ans = NA
    if (pm == 1) return(.hyp1Mode(alpha, beta, delta, mu))
    if (pm == 2) return(.hyp2Mode(alpha, beta, delta, mu))
    if (pm == 3) return(.hyp3Mode(alpha, beta, delta, mu))
    if (pm == 4) return(.hyp4Mode(alpha, beta, delta, mu))  
}
  

# ------------------------------------------------------------------------------


.rghyp = 
function(n, theta)
{   # A function implemented by Diethelm Wuertz

    # Author:
    #   Original Version by David Scott

    # FUNCTION:
    
    # Settings:
    lambda = theta[1]
    alpha = theta[2]
    beta = theta[3]
    delta = theta[4]
    mu = theta[5]
    chi = delta^2
    psi = alpha^2 - beta^2

    # Ckecks:
    if (alpha <= 0) stop("alpha must be greater than zero")  
    if (delta <= 0) stop("delta must be greater than zero")
    if (abs(beta) >= alpha) stop("abs value of beta must be less than alpha")
    
    # Random Numbers:
    if (lambda == 1){
        X = .rgigjd1(n, c(lambda, chi, psi))
    } else{
        X = .rgigjd(n, c(lambda, chi, psi))
    }
    
    # Result:
    sigma = sqrt(X)
    Z = rnorm(n)
    Y = mu + beta*sigma^2 + sigma*Z
    
    # Return Value:
    Y
}


# ------------------------------------------------------------------------------


.rgigjd = 
function(n, theta)
{   # A function implemented by Diethelm Wuertz

    # Author:
    #   Original Version by David Scott

    # FUNCTION:
    
    # Settings:
    lambda = theta[1]
    chi = theta[2]
    psi = theta[3]
    
    # Checks:
    if (chi < 0) stop("chi can not be negative")
    if (psi < 0) stop("psi can not be negative")
    if ((lambda >= 0)&(psi==0)) stop("When lambda >= 0, psi must be > 0")
    if ((lambda <= 0)&(chi==0)) stop("When lambda <= 0, chi must be > 0")
    if (chi == 0) stop("chi = 0, use rgamma")
    if (psi == 0) stop("algorithm only valid for psi > 0")
    
    alpha = sqrt(psi/chi)
    beta = sqrt(psi*chi)
    
    m = (lambda-1+sqrt((lambda-1)^2+beta^2))/beta
    
    g = function(y){
        0.5*beta*y^3 - y^2*(0.5*beta*m+lambda+1) +
            y*((lambda-1)*m-0.5*beta) + 0.5*beta*m
    }
    
    upper = m
    while (g(upper) <= 0) upper = 2*upper
    yM = uniroot(g, interval=c(0,m))$root
    yP = uniroot(g, interval=c(m,upper))$root
    
    a = (yP-m)*(yP/m)^(0.5*(lambda-1))*exp(-0.25*beta*(yP+1/yP-m-1/m))
    b = (yM-m)*(yM/m)^(0.5*(lambda-1))*exp(-0.25*beta*(yM+1/yM-m-1/m))
    c = -0.25*beta*(m+1/m) + 0.5*(lambda-1)*log(m)
    
    output = numeric(n)
    
    for(i in 1:n){
        need.value = TRUE
        while(need.value==TRUE){
            R1 = runif (1)
            R2 = runif (1)
            Y = m + a*R2/R1 + b*(1-R2)/R1
            if (Y>0){
                if (-log(R1)>=-0.5*(lambda-1)*log(Y)+0.25*beta*(Y+1/Y)+c){
                    need.value = FALSE
                }
            }
        }
        output[i] = Y
    }
    
    # Return Value:
    output/alpha
}


# ------------------------------------------------------------------------------


.rgigjd1 = 
function(n, theta)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Modified version of rgigjd to generate random observations
    #   from a generalised inverse Gaussian distribution in the
    #   special case where lambda = 1.
    
    # Author:
    #   Original Version by David Scott
 
    # FUNCTION:
    
    if (length(theta) == 2) theta = c(1, theta)
    
    # Settings:
    lambda = 1
    chi = theta[2]
    psi = theta[3]
    
    # Checks:
    if (chi < 0) stop("chi can not be negative")
    if (psi < 0) stop("psi can not be negative")    
    if (chi == 0) stop("chi = 0, use rgamma")
    if (psi == 0) stop("When lambda >= 0, psi must be > 0")
    
    alpha = sqrt(psi/chi)
    beta = sqrt(psi*chi)
    m = abs(beta)/beta
    g = function(y){
        0.5*beta*y^3 - y^2*(0.5*beta*m+lambda+1) +
            y*(-0.5*beta) + 0.5*beta*m
    }
    
    upper = m
    while (g(upper)<=0) upper = 2*upper
    yM = uniroot(g,interval=c(0,m))$root
    yP = uniroot(g,interval=c(m,upper))$root
    
    a = (yP-m)*exp(-0.25*beta*(yP+1/yP-m-1/m))
    b = (yM-m)*exp(-0.25*beta*(yM+1/yM-m-1/m))
    c = -0.25*beta*(m+1/m)
    
    output = numeric(n)
    
    for(i in 1:n){
        need.value = TRUE
        while(need.value==TRUE){
            R1 = runif (1)
            R2 = runif (1)
            Y = m + a*R2/R1 + b*(1-R2)/R1
            if (Y>0){
                if (-log(R1)>=0.25*beta*(Y+1/Y)+c){
                    need.value = FALSE
                }
            }
        }
        output[i] = Y
    }
    
    # Return Value:
    output/alpha
}


# ------------------------------------------------------------------------------


.dhyp1 = 
function(x, alpha = 1, beta = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Hyperbolic Density Function PDF
       
    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Density:
    efun = exp( -alpha*sqrt(delta^2 + (x-mu)^2) + beta*(x-mu) )
    sqr = sqrt(alpha^2-beta^2)
    bK1 = besselK(delta*sqr, nu = 1) 
    prefac = sqr / ( 2 * alpha * delta * bK1)
    ans = prefac * efun

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.dhyp2 =
function(x, zeta = 1, rho = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Hyperbolic density in the 2nd parameterization

    # FUNCTION:
    
    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter
    
    # Parameter Change:
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = dhyp(x, alpha, beta, delta, mu)
    ans
}


# ------------------------------------------------------------------------------


.dhyp3 = 
function(x, xi = 1/sqrt(2), chi = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Hyperbolic density in the 2nd parameterization
    
    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Parameter Change:
    rho = chi / xi
    zeta = 1/xi^2 - 1   
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = dhyp(x, alpha, beta, delta, mu)
    ans 
}


# ------------------------------------------------------------------------------

 
.dhyp4 = 
function(x, a.bar = 1, b.bar = 0, delta = 1, mu = 0)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns Hyperbolic density in the 2nd parameterization
    
    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Parameter Change:
    alpha = a.bar / delta
    beta = b.bar / delta
    
    # Return Value:
    ans = dhyp(x, alpha, beta, delta, mu)
    ans
}

  
# ------------------------------------------------------------------------------


.phyp1 = 
function(q, alpha = 1, beta = 0, delta = 1, mu = 0, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Return cumulative probability of Hyperbolic PDF
     
    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Cumulative Probability:
    ans = NULL
    for (Q in q) {
        Integral = integrate(dhyp, -Inf, Q, stop.on.error = FALSE, 
            alpha = alpha, beta = beta, delta = delta, mu = mu, ...)
        # Works in both, R and SPlus:
        ans = c(ans, as.numeric(unlist(Integral)[1]) ) 
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.phyp2 =
function(q, zeta = 1, rho = 0, delta = 1, mu = 0, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns cumulative probability in the 2nd parameterization
    
    # Arguments:
    #   zeta, rho - Shape Parameter, resulting |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Parameter Change:
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = phyp(q, alpha, beta, delta, mu, ...)
    ans
}


# ------------------------------------------------------------------------------


.phyp3 = 
function(q, xi = 1/sqrt(2), chi = 0, delta = 1, mu = 0, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns cumulative probability in the 3rd parameterization
    
    # Arguments:
    #   xi, xhi - Shape Parameter, resulting |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Parameter Change:
    rho = chi / xi
    zeta = 1/xi^2 - 1   
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = phyp(q, alpha, beta, delta, mu, ...)
    ans
}


# ------------------------------------------------------------------------------

 
.phyp4 = 
function(q, a.bar = 1, b.bar = 0, delta = 1, mu = 0, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns cumulative probability in the 4th parameterization
    
    # Arguments:
    #   a.bar, b.bar - Shape Parameter, resulting |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Parameter Change:
    alpha = a.bar / delta
    beta = b.bar / delta
    
    # Return Value:
    ans = phyp(q, alpha, beta, delta, mu, ...)
    ans
}


# ------------------------------------------------------------------------------


.qhyp1 = 
function(p, alpha = 1, beta = 0, delta = 1, mu = 0, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns quantiles of Hyperbolic PDF
    
    # Arguments:
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # Note:
    #   This procedure will not run under Splus.
  
    # FUNCTION:
    
    # Internal Functions:
    .froot <- 
    function(x, alpha, beta, delta, p) 
    {
        phyp(q = x, alpha = alpha, beta = beta, delta = delta, mu = 0) - p 
    }
    
    # Loop over all p's:            
    result = NULL   
    for (pp in p) {
        lower = -1
        upper = +1          
        counter = 0
        iteration = NA
        while (is.na(iteration)) {
            iteration = .unirootNA(f = .froot, interval = c(lower, upper), 
                alpha = alpha, beta = beta, delta = delta, p = pp)
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


.qhyp2 =
function(p, zeta = 1, rho = 0, delta = 1, mu = 0, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns quantiles of Hyperbolic PDF in the 2nd parameterization
    
    # Arguments:
    #   zeta, rho - Shape Parameter, resulting |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter
    
    # FUNCTION:
    
    # Parameter Change:
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = qhyp(p, alpha, beta, delta, mu, ...)
    ans
}


# ------------------------------------------------------------------------------


.qhyp3 = 
function(p, xi = 1/sqrt(2), chi = 0, delta = 1, mu = 0, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns quantiles of Hyperbolic PDF in the 3rd parameterization
    
    # Arguments:
    #   zeta, chi - Shape Parameter, resulting |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter
  
    # FUNCTION:
    
    # Parameter Change:
    rho = chi / xi
    zeta = 1/xi^2 - 1   
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = qhyp(p, alpha, beta, delta, mu, ...)
    ans
}

 
# ------------------------------------------------------------------------------


.qhyp4 = 
function(p, a.bar = 1, b.bar = 0, delta = 1, mu = 0, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns quantiles of Hyperbolic PDF in the 4th parameterization
    
    # Arguments:
    #   a.bar, b.bar - Shape Parameter, resulting |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Parameter Change:
    alpha = NA
    beta = NA
    
    # Return Value:
    ans = qhyp(p, alpha, beta, delta, mu, ...)
    ans
}   


# ------------------------------------------------------------------------------


.rhyp1 = 
function (n, alpha = 1, beta = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns random deviates of Hyperbolic PDF
    
    # Arguments:
    #   n - number of random deviates to be generated
    #   alpha, beta - Shape Parameter, |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter
    
    # Notes:
    #   I have removed my original Fortran program and replaced it by
    #   the dhyperb() function from the HyperbolicDist Package, written
    #   by David Scott, Ai-Wei Lee, Jennifer Tso, Richard Trendall.
    #   License: GPL

    # FUNCTION:
    
    # Result - Use Standard Parameterization:
    Zeta = delta * sqrt(alpha^2 - beta^2)
    hyp.Pi = beta / sqrt(alpha^2 - beta^2)
    theta = c(hyp.Pi, Zeta, delta, mu)

    # Return Value:
    ans = .rhyperb(n = n, theta = theta)
    ans
}


# ------------------------------------------------------------------------------
 

.rhyperb <- 
function (n, theta) 
{
    # Internal Function:
    
    hyp.pi = theta[1]
    zeta = theta[2]
    delta = theta[3]
    mu = theta[4]
    alpha = as.numeric(.hyperb.change.pars(1, 2, theta))[1] * delta
    beta = as.numeric(.hyperb.change.pars(1, 2, theta))[2] * delta
    phi = as.numeric(.hyperb.change.pars(1, 3, theta))[1] * delta
    gamma = as.numeric(.hyperb.change.pars(1, 3, theta))[2] * delta
    theta.start = -sqrt(phi * gamma)
    t = -sqrt(gamma/phi)
    w = sqrt(phi/gamma)
    delta1 = exp(theta.start)/phi
    delta2 = (w - t) * exp(theta.start)
    delta3 = exp(-gamma * w)/gamma
    k = 1/(delta1 + delta2 + delta3)
    r = k * delta1
    v = 1 - k * delta3
    output = numeric(n)
    need.value = TRUE
    for (i in 1:n) {
        while (need.value == TRUE) {
            U = runif(1)
            E = rexp(1)
            if (U <= r) {
                x = 1/phi * log(phi * U/k)
                if (E >= alpha * (sqrt(1 + x^2) + x)) {
                  need.value = FALSE } }
            if ((U > r) & (U <= v)) {
                x = t - 1/phi + U * exp(-theta.start)/k
                if (E >= alpha * sqrt(1 + x^2) - beta * x + theta.start) {
                    need.value = FALSE
                } 
            }
            if (U > v) {
                x = 1/gamma * log(k/gamma) - 1/gamma * log(1 - U)
                if (E >= alpha * (sqrt(1 + x^2) - x)) {
                    need.value = FALSE 
                } 
            } 
        }
        output[i] = delta * x + mu
        need.value = TRUE 
    }
    output 
}
    
    
# ------------------------------------------------------------------------------  
 

.hyperb.change.pars <-
function (from, to, theta) 
{
    # Internal Function:
    
    delta <- theta[3]
    mu <- theta[4]
    hyperb.pi <- theta[1]
    zeta <- theta[2] 
    if (from == 1 && to == 2) {
        alpha <- zeta * sqrt(1 + hyperb.pi^2)/delta
        beta <- zeta * hyperb.pi/delta
        output = c(alpha = alpha, beta = beta, delta = delta, mu = mu) 
    }
    if (from == 1 && to == 3) {
        phi <- zeta/delta * (sqrt(1 + hyperb.pi^2) + hyperb.pi)
        gamma <- zeta/delta * (sqrt(1 + hyperb.pi^2) - hyperb.pi)
        output = c(phi = phi, gamma = gamma, delta = delta, mu = mu)
    }
    output 
}


# ------------------------------------------------------------------------------


.rhyp2 =
function(n, zeta = 1, rho = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns random deviates of Hyperbolic PDF in the 2nd parameterization
    
    # Arguments:
    #   zeta, rho - Shape Parameter, resulting |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Parameter Change:
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = rhyp(n, alpha, beta, delta, mu)
    ans
}


# ------------------------------------------------------------------------------


.rhyp3 = 
function(n, xi = 1/sqrt(2), chi = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns random deviates of Hyperbolic PDF in the 3rd parameterization
    
    # Arguments:
    #   zeta, chi - Shape Parameter, resulting |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter

    # FUNCTION:
    
    # Parameter Change:
    rho = chi / xi
    zeta = 1/xi^2 - 1   
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = rhyp(n, alpha, beta, delta, mu)
    ans
}

 
# ------------------------------------------------------------------------------


.rhyp4 = 
function(n, a.bar = 1, b.bar = 0, delta  = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns random deviates of Hyperbolic PDF in the 4th parameterization
    
    # Arguments:
    #   a.bar, b.bar - Shape Parameter, resulting |beta| <= alpha
    #   delta  - Scale Parameter, 0 <= delta
    #   mu - Location Parameter
    
    # FUNCTION:
    
    # Parameter Change:
    alpha = a.bar / delta
    beta = b.bar / delta
    
    # Return Value:
    ans = rhyp(n, alpha, beta, delta, mu)
    ans
}   


# ------------------------------------------------------------------------------


.hyp1Mode =
function(alpha = 1, beta = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the mode of the Hyperbolic PDF

    # FUNCTION:
    
    # Mode:
    ans = mu + delta * beta / sqrt(alpha^2 - beta^2)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.hyp2Mode =
function(zeta = 1, rho = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the hyperbolic mode in the 2nd parameterization

    # FUNCTION:
    
    # Parameter Change:
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = hypMode(alpha, beta, delta, mu)
    ans
}


# ------------------------------------------------------------------------------


.hyp3Mode = 
function(xi = 1/sqrt(2), chi = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the hyperbolic mode in the 3rd parameterization

    # FUNCTION:
    
    # Parameter Change:
    rho = chi / xi
    zeta = 1/xi^2 - 1   
    alpha = zeta / ( delta * sqrt(1 - rho*rho) )
    beta = alpha * rho
    
    # Return Value:
    ans = hypMode(alpha, beta, delta, mu)
    ans
}


# ------------------------------------------------------------------------------


.hyp4Mode = 
function(a.bar = 1, b.bar = 0, delta  = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the hyperbolic mode in the 4th parameterization

    # FUNCTION:
    
    # Parameter Change:
    alpha = a.bar / delta
    beta = b.bar / delta
    
    # Return Value:
    ans = hypMode(alpha, beta, delta, mu)
    ans
}   


################################################################################
# FUNCTION:             DESCRIPTION:
#  dnig                  Returns density for inverse Gaussian DF
#  pnig                  Returns probability for for inverse Gaussian DF
#  qnig                  Returns quantiles for for inverse Gaussian DF 
#  rnig                  Returns random variates for inverse Gaussian DF


dnig = 
function(x, alpha = 1, beta = 0, delta = 1, mu = 0, log = FALSE)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Density:
    dgh(x = x, alpha = alpha, beta = beta, delta = delta, mu = mu, 
        lambda = -0.5, log = log)
}


# ------------------------------------------------------------------------------


pnig = 
function(q, alpha = 1, beta = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz

    # Function:
    
    # Probability:
    pgh(q = q, alpha = alpha, beta = beta, delta = delta, mu = mu, 
        lambda = -0.5)
}


# ------------------------------------------------------------------------------


qnig = 
function(p, alpha = 1, beta = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Quantiles
    qgh(p = p, alpha = alpha, beta = beta, delta = delta, mu = mu, 
        lambda = -0.5)
}


# ------------------------------------------------------------------------------


rnig = 
function(n, alpha = 1, beta = 0, delta = 1, mu = 0)
{   # A function implemented by Diethelm Wuertz

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
# FUNCTION:             DESCRIPTION:
#  nigShapeTriangle      Plots NIG Shape Triangle

   
nigShapeTriangle =
function(object, add = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots NIG Shape Triangle
    
    # Arguments:
    #   object - an object of class 'fDISTFIT' as returned by the
    #       function nigFit()
    
    # Example:
    #   nigShapeTriangle(nigFit(rnig(100), doplot = FALSE))
    
    # FUNCTION:
    
    # Settings:
    stopifnot(class(object) == "fDISTFIT")
    
    # Plot Frame:
    if (!add) {
        x = c(-1, 0, 1, -1)
        y = c( 1, 0, 1,  1)
        plot(x, y, type = "l", 
            xlab = "Asymmetry: chi", ylab = "Steepness: zeta")
        title(main = "NIG Shape Traingle")
        for (s in c(0.8, 0.6, 0.4, 0.2)) 
            lines(c(-s, s), c(s, s), lty = 3, col = "grey")  
        lines(c(0, 0), c(0, 1), lty = 3, col = "grey")
    }
    
    # Transform:
    par = object@fit$estimate
    names(par) = c("alpha", "beta", "delta", "mu")
    alpha = par[1] 
    beta = par[2]
    delta = par[3]
    mu = par[4]   
    
    # Add Points:
    zeta = 1/sqrt(1+delta*sqrt(alpha^2-beta^2))
    chi = zeta*(beta/alpha)
    points(chi, zeta, pch = 19, ...)
    
    # Result:
    ans = list(chi = chi[[1]], zeta = zeta[[1]])
    attr(ans, "control")<-par
    
    # Return Value:
    ans
}


################################################################################
# dght


dght =
function(x, beta = 1e-6, delta = 1, mu = 0, nu = 10, log = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Hyperbolic Distribution - Skew Symmaetric Student-t:
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


################################################################################
# FUNCTION:             DESCRIPTION:
#  hypSlider             Displays hyperbolic distribution function
#  nigSlider             Displays normal inverse Gausssian distribution function


hypSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # Hyperbolic Distribution:
    #   dhyp(x, alpha = 1, beta = 0, delta = 1, mu = 0, pm = c(1, 2, 3, 4))
        
    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N     = .sliderMenu(no = 1)
        alpha = .sliderMenu(no = 2)
        beta  = .sliderMenu(no = 3)
        delta = .sliderMenu(no = 4)
        mu    = .sliderMenu(no = 5)
        pm    = .sliderMenu(no = 6)
        
        # Plot Data:     
        xmin = round(qhyp(0.01, alpha, beta, delta, mu, pm), digits = 2)
        xmax = round(qhyp(0.99, alpha, beta, delta, mu, pm), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dhyp(s, alpha, beta, delta, mu, pm)
        y2 = phyp(s, alpha, beta, delta, mu, pm)
        main1 = paste("HYP Density\n", 
            "alpha = ", as.character(alpha), " | ",
            "beta = ", as.character(beta), " | ",
            "delta = ", as.character(delta), " | ",
            "mu = ", as.character(mu) )
        main2 = paste("HYP Probability\n",
            "xmin 0.01% = ", as.character(xmin), " | ",
            "xmax 0.99% = ", as.character(xmax), " | ",
            "pm = ", as.character(pm) )      
         
        # Frame
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
        abline(v = mu, lty = 3, col = "red")
        title(main = main2)       
        
        # Reset Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c( "N","alpha","beta","delta", "mu","pm"),
       minima =      c(  50,  0.00, -2.00,   0.00, -5.0,   1),
       maxima =      c(1000,  2.00, +2.00,   5.00, +5.0,   4),
       resolutions = c(  50,  0.20,  0.20,   1.00,  1.0,   1),
       starts =      c(  50,  1.00,  0.00,   1.00,  0.0,   1))
}


# ------------------------------------------------------------------------------


nigSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Distribution:
    #   dnig(x, alpha = 1, beta = 0, delta = 1, mu = 0) 
   
    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N     = .sliderMenu(no = 1)
        alpha = .sliderMenu(no = 2)
        beta  = .sliderMenu(no = 3)
        delta = .sliderMenu(no = 4)
        mu    = .sliderMenu(no = 5)
        
        # Plot Data:      
        xmin = round(qnig(0.01, alpha, beta, delta, mu), digits = 2)
        xmax = round(qnig(0.99, alpha, beta, delta, mu), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dnig(s, alpha, beta, delta, mu)
        y2 = pnig(s, alpha, beta, delta, mu)
        main1 = paste("NIG Density\n", 
            "alpha = ", as.character(alpha), " | ",
            "beta = ", as.character(beta), " | ",
            "delta = ", as.character(delta), " | ",
            "mu = ", as.character(mu))
        main2 = paste("NIG Probability\n",
            "xmin 0.01% = ", as.character(xmin), " | ",
            "xmax 0.99% = ", as.character(xmax), " | ")       
      
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
        abline(v = mu, lty = 3, col = "red")
        title(main = main2)     
        
        # Frame:
        par(mfrow = c(1, 1), cex = 0.7)
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c( "N", "alpha", "beta", "delta", "mu"),
       minima =      c(  50,   0.00,   -2.00,    0.00, -5.0),
       maxima =      c(1000,   2.00,   +2.00,   10.00, +5.0),
       resolutions = c(  50,   0.20,    0.20,    1.00,  1.0),
       starts =      c(  50,   1.00,    0.00,    1.00,  0.0))
}


################################################################################

