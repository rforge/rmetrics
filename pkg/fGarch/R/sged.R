
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
# FUNCTION:              GED DISTRIBUTION:
#  dged                   Density for the Generalized Error Distribution
#  pged                   Probability function for the GED
#  qged                   Quantile function for the GED
#  rged                   Random Number Generator for the GED
# FUNCTION:              SKEW GED DISTRIBUTION:
#  dsged                  Density for the skewed GED
#  psged                  Probability function for the skewed GED
#  qsged                  Quantile function for the skewed GED
#  rsged                  Random Number Generator for the skewed GED
# FUNCTION:              PARAMETER ESTIMATION:
#  gedFit                 Fit the parameters for a GED distribution
#  sgedFit                Fit the parameters for a skew GED distribution
# FUNCTION:              SLIDER:
#  .sgedSlider            Displays Generalized Error Distribution and RVS
################################################################################


dged =
function(x, mean = 0, sd = 1, nu = 2)
{   # A function imlemented by Diethelm Wuertz

    # Description:
    #   Compute the density for the 
    #   generalized error distribution.
    
    # FUNCTION:
    
    # Compute Density:
    z = (x - mean ) / sd
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
    result = g * exp (-0.5*(abs(z/lambda))^nu) / sd
    
    # Return Value
    result
}

        
# ------------------------------------------------------------------------------


pged = 
function(q, mean = 0, sd = 1, nu = 2)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Compute the probability for the  
    #   generalized error distribution.
    
    # FUNCTION:
        
    # Compute Probability:
    q = (q - mean ) / sd
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
    h = 2^(1/nu) * lambda * g * gamma(1/nu) / nu
    s = 0.5 * ( abs(q) / lambda )^nu
    result = 0.5 + sign(q) * h * pgamma(s, 1/nu)
    
    # Return Value:
    result
}
        

# ------------------------------------------------------------------------------


qged =
function(p, mean = 0, sd = 1, nu = 2)
{   # A function implemented by Diethelm Wuertz
        
    # Description:
    #   Compute the quantiles for the  
    #   generalized error distribution.
    
    # FUNCTION:
    
    # Compute Quantiles:
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    q = lambda * (2*qgamma((abs(2*p-1)), 1/nu))^(1/nu)
    result = q*sign(2*p-1) * sd + mean
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------

    
rged = 
function(n, mean = 0, sd = 1, nu = 2)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Generate GED random deviates. The function uses the 
    #   method based on the transformation of a Gamma random 
    #   variable.
    
    # FUNCTION:
    
    # Generate Random Deviates:
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    # print(lambda)
    r = rgamma(n, 1/nu)
    z =  lambda * (2*r)^(1/nu) * sign(runif(n)-1/2)
    result = z * sd + mean

    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.dsged = 
function(x, nu, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Standardize:
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
    m1 = 2^(1/nu) * lambda * gamma(2/nu) / gamma(1/nu)
    mu = m1*(xi-1/xi)
    sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    z = x*sigma + mu  
    
    # Compute:
    Xi = xi^sign(z)
    g = 2  / (xi + 1/xi)    
    Density = g * dged(x = z/Xi, nu=nu)  
    
    # Return Value:
    Density * sigma 
}

      
dsged =
function(x, mean = 0, sd = 1, nu = 2, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the density function of the 
    #   skewed generalized error distribution
    
    # FUNCTION:
        
    # Shift and Scale:
    result = .dsged(x = (x-mean)/sd, nu = nu, xi = xi) / sd
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.psged =
 function(q, nu, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Standardize:
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
    m1 = 2^(1/nu) * lambda * gamma(2/nu) / gamma(1/nu)
    mu = m1*(xi-1/xi)
    sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    z = q*sigma + mu
    
    # Compute:  
    Xi = xi^sign(z)
    g = 2  / (xi + 1/xi)    
    Probability = Heaviside(z) - sign(z) * g * Xi * pged(q = -abs(z)/Xi, nu=nu)
    
    # Return Value:
    Probability 
}

      
psged =
function(q, mean = 0, sd = 1, nu = 2, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the distribution function of the 
    #   skewed generalized error distribution
    
    # FUNCTION:
              
    # Shift and Scale:
    result = .psged(q = (q-mean)/sd, nu = nu, xi = xi)
          
    # Return Value:
    result
}


# ------------------------------------------------------------------------------    


.qsged =
function(p, nu, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Standardize:
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
    m1 = 2^(1/nu) * lambda * gamma(2/nu) / gamma(1/nu)
    mu = m1*(xi-1/xi)
    sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    
    # Compute:  
    g = 2  / (xi + 1/xi)
    sig = sign(p-1/2) 
    Xi = xi^sig       
    p = (Heaviside(p-1/2)-sig*p) / (g*Xi)
    Quantile = (-sig*qged(p=p, sd=Xi, nu=nu) - mu ) / sigma
    
    # Return Value:
    Quantile 
}

        
qsged =
function(p, mean = 0, sd = 1, nu = 2, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the quantile function of the 
    #   skewed generalized error distribution
    
    # FUNCTION:
        
    # Shift and Scale:
    result = .qsged(p = p, nu = nu, xi = xi) * sd + mean
    
    # Return Value:
    result
}

    
# ------------------------------------------------------------------------------
    

.rsged =
function(n, nu, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Generate Random Deviates:
    weight = xi / (xi + 1/xi)
    z = runif(n, -weight, 1-weight)
    Xi = xi^sign(z)
    Random = -abs(rged(n, nu=nu))/Xi * sign(z)  
    
    # Scale:
    lambda = sqrt ( 2^(-2/nu) * gamma(1/nu) / gamma(3/nu) )
    g  = nu / ( lambda * (2^(1+1/nu)) * gamma(1/nu) )
    m1 = 2^(1/nu) * lambda * gamma(2/nu) / gamma(1/nu)
    mu = m1*(xi-1/xi)
    sigma =  sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    Random = (Random - mu ) / sigma 
    
    # Return value:
    Random 
}


# ------------------------------------------------------------------------------


rsged =
function(n, mean = 0, sd = 1, nu = 2, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Generate random deviates from the 
    #   skewed generalized error distribution
    
    # FUNCTION:
        
    # Shift and Scale:
    result = .rsged(n = n, nu = nu, xi = xi) * sd + mean
    
    # Return Value:
    result
}


################################################################################


gedFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fit the parameters for a GED distribution
    
    # FUNCTION:
    
    # For S-Plus compatibility:
    if (!exists("nlm")) 
        nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
        
    # Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 2)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dged(y, x[1], x[2], x[3])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


# ------------------------------------------------------------------------------


sgedFit =
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Fit the parameters for a skew GED distribution
    
    # FUNCTION:
    
    # For S-Plus compatibility:
    if (!exists("nlm")) 
        nlm = function (f, p, ...) nlminb(start = p, objective = f, ...) 
        
    # Start Value:
    p = c(mean = mean(x), sd = sqrt(var(x)), nu = 2, xi = 1)

    # Log-likelihood Function:
    loglik = function(x, y = x){ 
        f = -sum(log(dsged(y, x[1], x[2], x[3], x[4])))
        f }
        
    # Minimization:
    fit = nlm(f = loglik, p = p, y = x, ...)
    
    # Return Value:
    fit
}   


################################################################################


sgedSlider = 
function(type = c("dist", "rand"))
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays interactively skew GED distribution
    
    # Note:
    #   dsged(x, mean = 0, sd = 1, nu = 5, xi = 1.5)
    
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
        xmin = round(qsged(0.01, mean, sd, nu, xi), digits = 2)
        xmax = round(qsged(0.99, mean, sd, nu, xi), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dsged(s, mean, sd, nu, xi)
        y2 = psged(s, mean, sd, nu, xi)
        main1 = paste("Skew GED Density\n", 
            "mean = ", as.character(mean), " | ",
            "sd = ", as.character(sd), " | ",
            "nu = ", as.character(nu), " | ",
            "xi = ", as.character(xi) )
        main2 = paste("Skew GED Probability\n",
            "xmin [0.01] = ", as.character(xmin), " | ",
            "xmax [0.99] = ", as.character(xmax) )   
            
        # Random Numbers:
        if (type[1] == "rand") {
            x = rsged(N, mean, sd, nu, xi)
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
       names =       c(   "N", "mean",  "sd",  "nu", "xi", "xi.inv"),
       minima =      c(   10,    -5.0,   0.1,   2.1,  1.0,       0 ),
       maxima =      c( 1000,    +5.0,   5.0,  10.0, 10.0,       1 ),
       resolutions = c(   10,     0.1,   0.1,   0.1,  0.1,       1 ),
       starts =      c(  100,     0.0,   1.0,   5.0,  1.0,       0 )
    )
}


################################################################################

