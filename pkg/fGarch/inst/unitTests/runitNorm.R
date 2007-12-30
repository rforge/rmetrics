
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
# FUNCTION:              SKEW NORMAL DISTRIBUTION:
#  dsnorm                 Density for the skew normal Distribution
#  psnorm                 Probability function for the skew NORM
#  qsnorm                 Quantile function for the skew NORM
#  rsnorm                 Random Number Generator for the skew NORM
# FUNCTION:              PARAMETER ESTIMATION:
#  normFit                Fit the parameters for a Normal distribution
#  snormFit               Fit the parameters for a skew Normal distribution
# FUNCTION:              SLIDER:
#  snormSlider           Displays Normal Distribution and RVS
################################################################################


test.normDist = 
function()
{   
    # Normal Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("norm",  mean = 0, sd = 1, robust = FALSE)
    print(test)
    checkTrue(sum(test) == 3)                                     
    
    # Skew Normal Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    test = .distCheck("snorm", mean = 0, sd = 1, xi = 1.5, robust = FALSE) 
    print(test)
    checkTrue(sum(test) == 3)                                      
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.normSlider = 
function()
{   
    # Try Distribution:
    # snormSlider(type = "dist")
    NA
   
    # Try Random Variates:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    # snormSlider(type = "rand")
    NA 
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.normFit = 
function()
{  
    # Parameter Estimation:
    #  normFit - Fit the parameters for a Normal distribution
    
    # Normal Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rnorm(n = 1000, mean = 0, sd = 1)
    fit = normFit(x)
    print(fit)
    target = round(as.vector(fit$estimate), 4)
    print(target)
    current = c(0.0114, 0.9904)
    checkEqualsNumeric(target, current)

      
    # Parameter Estimation:
    #  snormFit - Fit the parameters for a skew Normal distribution
    
    # Skew Normal Distribution:
    RNGkind(kind = "Marsaglia-Multicarry", normal.kind = "Inversion")
    set.seed(4711, kind = "Marsaglia-Multicarry")
    x = rsnorm(n = 1000, mean = 0, sd = 1, xi = 1.5)
    fit = snormFit(x)
    print(fit)
    target = round(as.vector(fit$estimate), 4)
    print(target)
    current = c(-0.0017, 1.0259, 1.5632)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


################################################################################
    
