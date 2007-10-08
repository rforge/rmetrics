
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
# FUNCTION:             STABLE SLIDERS:
#  stableSlider          Displays stable distribution function
################################################################################


stableSlider =  
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays the stable distribution

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

