
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
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:              VARIANCE-1 STUDENT-T DISTRIBUTION:
#  dstd                   Density for the Student-t Distribution
#  pstd                   Probability function for the Student-t Distribution
#  qstd                   Quantile function for the Student-t Distribution
#  rstd                   Random Number Generator for the Student-t
# FUNCTION:              SKEW VARIANCE-1 STUDENT-T DISTRIBUTION:
#  dsstd                  Density for the skewed Student-t Distribution
#  psstd                  Probability function for the skewed STD
#  qsstd                  Quantile function for the skewed STD
#  rsstd                  Random Number Generator for the skewed STD
# FUNCTION:              PARAMETER ESTIMATION:
#  stdFit                 Fit the parameters for a Sudent-t distribution
#  sstdFit                Fit the parameters for a skew Sudent-t distribution
# FUNCTION:              SLIDER:
#  .stdSlider             Displays Variance-1 Student-t Distribution and RVS
################################################################################


dstd <- 
    function(x, mean = 0, sd = 1, nu = 5)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the density for the  
    #   Student-t distribution.
    
    # FUNCTION:
    
    # Compute Density:
    s = sqrt(nu/(nu-2))
    z = (x - mean) / sd
    # result = .Internal(dnt(x = z*s, df = nu, ncp = 0, log = FALSE)) / (sd/s)
    result = dt(x = z*s, df = nu) * s / sd
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


pstd <- 
    function (q, mean = 0, sd = 1, nu = 5)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the probability for the  
    #   Student-t distribution.
    
    # FUNCTION:
    
    # Compute Probability:
    s = sqrt(nu/(nu-2))
    z = (q - mean) / sd
    # result = .Internal(pnt(q = z*s, df = nu, ncp = 0, lower.tail = TRUE, 
    #   log.p = FALSE))
    result = pt(q = z*s, df = nu)
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


qstd <- 
    function (p, mean = 0, sd = 1, nu = 5)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the quantiles for the  
    #   Student-t distribution.
    
    # FUNCTION:
    
    # Compute Quantiles:
    s = sqrt(nu/(nu-2))
    # x = .Internal(qt(p = p, df = nu, lower.tail = TRUE, log.p = FALSE)) / s
    # result = x*sd + mean
    result = qt(p = p, df = nu) * sd / s + mean
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rstd <- 
    function(n, mean = 0, sd = 1, nu = 5)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Generate random deviates from the  
    #   Student-t distribution.
    
    # FUNCTION:
    
    # Generate Random Deviates:
    s = sqrt(nu/(nu-2))
    # result = .Internal(rt(n = n, df = nu)) * sd / s + mean
    result = rt(n = n, df = nu) * sd / s + mean 
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.dsstd <- 
    function(x, nu, xi) 
{   
    # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # For SPlus compatibility:
    if (!exists("beta"))
        beta <<- function (a, b) exp( lgamma(a) + lgamma(b) -lgamma(a+b) )
    
    # Standardize:  
    m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
    mu = m1*(xi-1/xi)
    sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    z = x*sigma + mu  
    
    # Compute:
    Xi = xi^sign(z)
    g = 2 / (xi + 1/xi) 
    Density = g * dstd(x = z/Xi, nu = nu)  
    
    # Return Value:
    Density * sigma 
}


# ------------------------------------------------------------------------------

      
dsstd <- 
    function(x, mean = 0, sd = 1, nu = 5, xi = 1.5)
{   
    # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the density function of the 
    #   skewed Student-t distribution
    
    # FUNCTION:
    
    # Shift and Scale:
    result = .dsstd(x = (x-mean)/sd, nu = nu, xi = xi) / sd
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.psstd <- 
    function(q, nu, xi) 
{   
    # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # For SPlus compatibility:
    if (!exists("beta"))
        beta <<- function (a, b) exp( lgamma(a) + lgamma(b) -lgamma(a+b) )
        
    # Standardize:
    m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
    mu = m1*(xi-1/xi)
    sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    z = q*sigma + mu
    
    # Compute:  
    Xi = xi^sign(z)
    g = 2 / (xi + 1/xi) 
    Probability = Heaviside(z) - sign(z) * g * Xi * pstd(q = -abs(z)/Xi, nu = nu)
    
    # Return Value:
    Probability 
}


# ------------------------------------------------------------------------------

      
psstd <- 
    function(q, mean = 0, sd = 1, nu = 5, xi = 1.5)
{   
    # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the distribution function of the 
    #   skewed Student-t distribution
    
    # FUNCTION:
          
    # Shift and Scale:
    result = .psstd(q = (q-mean)/sd, nu = nu, xi = xi)
          
    # Return Value:
    result
}


# ------------------------------------------------------------------------------    


.qsstd <- 
    function(p, nu, xi) 
{   
    # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # For SPlus compatibility:
    if (!exists("beta"))
        beta <<- function (a, b) exp( lgamma(a) + lgamma(b) -lgamma(a+b) )
        
    # Standardize:
    m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
    mu = m1*(xi-1/xi)
    sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    
    # Compute:  
    g = 2  / (xi + 1/xi)
    sig = sign(p-1/2) 
    Xi = xi^sig       
    p = (Heaviside(p-1/2)-sig*p) / (g*Xi)
    Quantile = (-sig*qstd(p = p, sd = Xi, nu = nu) - mu ) / sigma
    
    # Return Value:
    Quantile 
}


# ------------------------------------------------------------------------------

    
qsstd <- 
    function(p, mean = 0, sd = 1, nu = 5, xi = 1.5)
{   
    # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the quantile function of the 
    #   skewed Student-t distribution
    
    # FUNCTION:
        
    # Shift and Scale:
    result = .qsstd(p = p, nu = nu, xi = xi) * sd + mean
    
    # Return Value:
    result
}

    
# ------------------------------------------------------------------------------


.rsstd <- 
    function(n, nu, xi) 
{   
    # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # For SPlus compatibility:
    if (!exists("beta"))
        beta <<- function (a, b) exp( lgamma(a) + lgamma(b) -lgamma(a+b) )
        
    # Generate Random Deviates:
    weight = xi / (xi + 1/xi)
    z = runif(n, -weight, 1-weight)
    Xi = xi^sign(z)
    Random = -abs(rstd(n, nu = nu))/Xi * sign(z)    
    
    # Scale:
    m1 = 2 * sqrt(nu-2) / (nu-1) / beta(1/2, nu/2)
    mu = m1*(xi-1/xi)
    sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    Random = (Random - mu ) / sigma 
    
    # Return value:
    Random 
}


# ------------------------------------------------------------------------------
    

rsstd <- 
    function(n, mean = 0, sd = 1, nu = 5, xi = 1.5)
{   
    # A function implemented by Diethelm Wuertz 

    # Description:
    #   Generate random deviates from the 
    #   skewed Student-t distribution
    
    # FUNCTION:
        
    # Shift and Scale:
    result = .rsstd(n = n, nu = nu, xi = xi) * sd + mean
    
    # Return Value:
    result
}


################################################################################


stdFit <-
function(x, ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fit the parameters for a skew Normal distribution
    
    # FUNCTION:
     
    # Start Value:
    start = c(mean = mean(x), sd = sqrt(var(x)), nu = 4)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dstd(y, x[1], x[2], x[3])))
        f }
        
    # Minimization:
    fit = nlminb(start = start, objective = loglik, 
        lower = c(-Inf, 0, 2), upper = c(Inf, Inf, Inf), y = x, ...)
        
    # Add Names to $par
    names(fit$par) = c("mean", "sd", "nu")
    
    # Return Value:
    fit
}    


# ------------------------------------------------------------------------------


sstdFit <- 
    function(x, ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fit the parameters for a skew Sudent-t distribution
    #   with unit variance
    
    # FUNCTION:
    
    # For S-Plus compatibility:
    if (!exists("nlm")) 
        nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
    
    # Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 4, xi = 1)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dsstd(y, x[1], x[2], x[3], x[4])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    Names = c("mean", "sd", "nu", "xi")
    names(fit$estimate) = Names
    names(fit$gradient) = Names
    
    # Return Value:
    fit
}   


################################################################################# ------------------------------------------------------------------------------


sstdSlider <- 
    function(type = c("dist", "rand"))
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays interactively skew Student-t distribution
    
    # Note:
    #   dsstd(x, mean = 0, sd = 1, nu = 5, xi = 1.5)
    
    
    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N      = .sliderMenu(no = 1)
        mean   = .sliderMenu(no = 2)
        sd     = .sliderMenu(no = 3)
        nu     = .sliderMenu(no = 4)
        xi     = .sliderMenu(no = 5)
        invert = .sliderMenu(no = 6)
        
        # Compute Data:  
        if (invert == 1) xi = round(1/xi, digits = 4)
        xmin = round(qsstd(0.01, mean, sd, nu, xi), digits = 2)
        xmax = round(qsstd(0.99, mean, sd, nu, xi), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dsstd(s, mean, sd, nu, xi)
        y2 = psstd(s, mean, sd, nu, xi)
        main1 = paste("Skew Student-t Density\n", 
            "mean = ", as.character(mean), " | ",
            "sd = ", as.character(sd), " | ",
            "nu = ", as.character(nu), " | ",
            "xi = ", as.character(xi) )
        main2 = paste("Skew Student-t Probability\n",
            "xmin [0.01] = ", as.character(xmin), " | ",
            "xmax [0.99] = ", as.character(xmax) )   
            
        # Random Numbers:
        if (type[1] == "rand") {
            x = rsstd(N, mean, sd, nu, xi) 
        }      
        
        # Frame:    
        par(mfrow = c(2, 1), cex = 0.7)
        
        # Density:
        if (type[1] == "rand") {
            hist(x, probability = TRUE, col = "steelblue", border = "white",
                breaks = "FD",
                xlim = c(xmin, xmax), ylim = c(0, 1.1*max(y1)), main = main1 )
            lines(s, y1, col = "orange")
        } else {
            plot(s, y1, type = "l", xlim = c(xmin, xmax), col = "steelblue")
            abline (h = 0, lty = 3)
            title(main = main1) 
            grid() 
        }
            
        # Probability:
        plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
            col = "steelblue" )
        abline (h = 0, lty = 3)
        title(main = main2) 
        grid()
        
        # Frame:
        par(mfrow = c(1, 1), cex = 0.7) 
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(   "N", "mean",  "sd",  "nu",  "xi", "xi.inv"),
       minima =      c(   10,   -5.0,    0.1,   2.1,   1.0,       0 ),
       maxima =      c(  500,   +5.0,    5.0,  10.0,  10.0,       1 ),
       resolutions = c(   10,    0.1,    0.1,   0.1,   0.1,       1 ),
       starts =      c(  100,    0.0,    1.0,   5.0,   1.0,       0 )
    )
}


################################################################################

