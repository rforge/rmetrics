
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
# FUNCTION:              NORMAL Distribution - part of R's base Package:
#  * dnorm                R: Density for the Normal Distribution
#  * pnorm                R: Probability function for the Normal Distribution
#  * qnorm                R: Quantile function for the Normal Distribution
#  * rnorm                R: Random Number Generator for the Normal Distribution  
# FUNCTION:              SKEW NORMAL DISTRIBUTION:
#  dsnorm                 Density for the skew normal Distribution
#  psnorm                 Probability function for the skew NORM
#  qsnorm                 Quantile function for the skew NORM
#  rsnorm                 Random Number Generator for the skew NORM
#  .snormSlider           Displays Normal Distribution and RVS
################################################################################


.dsnorm = 
function(x, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the density function of the "normalized" skew 
    #   normal distribution
    
    # FUNCTION:

    # Standardize:
    m1 = 2/sqrt(2*pi)
    mu = m1 * (xi - 1/xi)
    sigma = sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    z = x*sigma + mu  
    # Compute:
    Xi = xi^sign(z)
    g = 2 / (xi + 1/xi) 
    Density = g * dnorm(x = z/Xi)  
    # Return Value:
    Density * sigma 
}


# ------------------------------------------------------------------------------
        

dsnorm =
function(x, mean = 0, sd = 1, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the density function of the skew normal distribution
    
    # Arguments:
    #   x - a numeric vector of quantiles.
    #   mean, sd, xi - location parameter, scale parameter, and 
    #       skewness parameter.
    
    # FUNCTION:
    
    # Shift and Scale:
    result = .dsnorm(x = (x-mean)/sd, xi = xi) / sd
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.psnorm =
function(q, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Standardize:
      m1 = 2/sqrt(2*pi)
      mu = m1 * (xi - 1/xi)
      sigma = sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
      z = q*sigma + mu
    # Compute:  
      Xi = xi^sign(z)
      g = 2  / (xi + 1/xi)  
      Probability = Heaviside(z) - sign(z) * g * Xi * pnorm(q = -abs(z)/Xi)
    # Return Value:
      Probability 
}
    
      
# ------------------------------------------------------------------------------
  

psnorm =
function(q, mean = 0, sd = 1, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the distribution function of the 
    #   skew normal distribution
    
    # Arguments:
    #   q - a numeric vector of quantiles.
    #   mean, sd, xi - location parameter, scale parameter, and 
    #       skewness parameter.
    
    # FUNCTION:
    
    # Shift and Scale:
    result = .psnorm(q = (q-mean)/sd, xi = xi)
          
    # Return Value:
    result
}


# ------------------------------------------------------------------------------    


.qsnorm =
function(p, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Standardize:
      m1 = 2/sqrt(2*pi)
      mu = m1 * (xi - 1/xi)
      sigma = sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
    # Compute:  
      g = 2  / (xi + 1/xi)
      sig = sign(p-1/2) 
      Xi = xi^sig         
      p = (Heaviside(p-1/2)-sig*p) / (g*Xi)
      Quantile = (-sig*qnorm(p = p, sd = Xi) - mu ) / sigma
    # Return Value:
      Quantile 
}
    
 
# ------------------------------------------------------------------------------

   
qsnorm =
function(p, mean = 0, sd = 1, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Compute the quantile function of the 
    #   skew normal distribution
    
    # Arguments:
    #   p - a numeric vector of probabilities.
    #   mean, sd, xi - location parameter, scale parameter, and 
    #       skewness parameter.
    
    # FUNCTION:
    
    # Shift and Scale:
    result = .qsnorm(p = p, xi = xi) * sd + mean
    
    # Return Value:
    result
}

    
# ------------------------------------------------------------------------------


.rsnorm =
function(n, xi) 
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Internal Function
    
    # FUNCTION:
    
    # Generate Random Deviates:
      weight = xi / (xi + 1/xi)
      z = runif(n, -weight, 1-weight)
      Xi = xi^sign(z)
      Random = -abs(rnorm(n))/Xi * sign(z)  
    # Scale:
      m1 = 2/sqrt(2*pi)
      mu = m1 * (xi - 1/xi)
      sigma = sqrt((1-m1^2)*(xi^2+1/xi^2) + 2*m1^2 - 1)
      Random = (Random - mu ) / sigma   
    # Return value:
      Random 
}
 

# ------------------------------------------------------------------------------
       

rsnorm =
function(n, mean = 0, sd = 1, xi = 1.5)
{   # A function implemented by Diethelm Wuertz 

    # Description:
    #   Generate random deviates from the 
    #   skew normal distribution
    
    # Arguments:
    #   n - an integer value giving the number of observation.
    #   mean, sd, xi - location parameter, scale parameter, and 
    #       skewness parameter.
    
    # FUNCTION:
    
    # Shift and Scale:
    result = .rsnorm(n = n, xi = xi) * sd + mean
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


.snormSlider = 
function(type = c("dist", "rand"))
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   dsnorm(x, mean = 0, sd = 1, xi = 1.5)
    
    # FUNCTION:
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N      = .sliderMenu(no = 1)
        mean   = .sliderMenu(no = 2)
        sd     = .sliderMenu(no = 3)
        xi     = .sliderMenu(no = 4)
        invert = .sliderMenu(no = 5)
        
        # Compute Data:  
        if (invert == 1) xi = 1/xi
        xmin = round(qsnorm(0.001, mean, sd, xi), digits = 2)
        xmax = round(qsnorm(0.999, mean, sd, xi), digits = 2)
        s = seq(xmin, xmax, length = N)
        y1 = dsnorm(s, mean, sd, xi)
        y2 = psnorm(s, mean, sd, xi)
        main1 = paste("Skew Normal Density\n", 
            "mean = ", as.character(mean), " | ",
            "sd = ", as.character(sd), " | ",
            "xi = ", as.character(xi) )
        main2 = paste("Skew Normal Probability\n",
            "xmin [0.001] = ", as.character(xmin), " | ",
            "xmax [0.999] = ", as.character(xmax) ) 
            
        # Random Numbers:
        if (type[1] == "rand") {
            x = rsnorm(N, mean, sd, xi) 
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
       names =       c(   "N", "mean",   "sd",  "xi", "xi.inv"),
       minima =      c(   10,    -5.0,    0.1,   1.0,       0 ),
       maxima =      c(  500,    +5.0,    5.0,  10.0,       1 ),
       resolutions = c(   10,     0.1,    0.1,   0.1,       1 ),
       starts =      c(  100,     0.0,    1.0,   1.0,       0 )
    )
}


################################################################################

