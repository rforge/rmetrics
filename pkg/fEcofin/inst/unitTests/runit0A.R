
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
# FUNCTION:                 INTERNAL USED PLOT FUNCTIONS: 
#  .residualsPlot            Returns a residual series plot
#  .acfPlot                  Returns a autocorrelation function plot
#  .pacfPlot                 Returns a partial ACF plot
#  .mrlPlot                  Returns a mean residual life plot
# FUNCTION:                 INTERNAL USED BIVARIATE PLOT FUNCTIONS:
#  .responsesPlot            Returns a response series plot
#  .firePlot                 Returns a fitted values vs.residuals plot
# FUNCTION:                 INTERNAL THREE-DIMENSIONAL PLOT UTILITIES:
#  .circlesPlot              Returns a circles plot indexing 3rd variable
#  .perspPlot                Returns a perspective plot in 2 dimensions
#  .contourPlot              Returns a contour plot in 2 dimensions
#  .histStack                Returns a stacked histogram plot
# FUNCTION:                 SLIDER MENU:
#  .sliderMenu               Starts a slider menu
# FUNCTION:                 SOME UTILITIES:
#  .description              Sets default description string
#  .unirootNA                Computes zero without error exit
#  .datax                    Loads timeSeries objects from demo files    
# FUNCTION:                 DATE FUNCTIONS:                   
#  .fjulian                  Transform formatted dates to julian day numbers    
#  .julian                   Implements SPlus like 'julian'                                                              
#  .isISO8601                Checks if the date/time is ISO8601 formatted                                               
#  .isPOSIX                  Checks for an object of class POSIX                                                        
#  .by2seconds               Converts 'by' string into numeric value of seconds
# FUNCTION:                 SOME METHODS:
#  .print                    Internal print method                              
#  .plot                     Internal plot method                               
#  .summary                  Internal summary method                            
#  .predict                  Internal predict method                            
################################################################################


test.internalPlotFunctions = 
function()
{
    #  .residualsPlot            Returns a residual series plot
    #  .acfPlot                  Returns a autocorrelation function plot
    #  .pacfPlot                 Returns a partial ACF plot
    #  .mrlPlot                  Returns a mean residual life plot

    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.bivariatePlotFunctions = 
function()
{
    #  .responsesPlot            Returns a response series plot
    #  .firePlot                 Returns a fitted values vs.residuals plot
    
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.threeDimPlotUtilities = 
function()
{
    #  .circlesPlot              Returns a circles plot indexing 3rd variable
    #  .perspPlot                Returns a perspective plot in 2 dimensions
    #  .contourPlot              Returns a contour plot in 2 dimensions
    #  .histStack                Returns a stacked histogram plot
    
    # Circles Plot:
    .circlesPlot(x = rnorm(50), y = rnorm(50), z = rnorm(50))
    title(main = "Circles Plot")
    # ... Note, x may be a vector, a list, a data.frame or a matrix.
     
    
    # Internal Perspective Plot with "Nice" Default Settings:
    # .perspPlot(x, y, z, theta = -40, phi = 30, col = "steelblue", ps = 9, ...) 
    x = y = seq(-10, 10, length= 30)
    f = function(x,y) { r = sqrt(x^2+y^2); 10 * sin(r)/r } 
    .perspPlot(x, y, z = outer(x, y, f))
    
    # Synonyme Contour Plot:
    .contourPlot(x, y, z = outer(x, y, f))

    # Stacked Histogram Plot:
    .histStack(rnorm(1000, -1), rnorm(1000, 1))

    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.sliderMenu = 
function()
{
    #  .sliderMenu               Starts a slider menu   
    
    # Slider Function:
    .normalSlider = 
    function()
    {   
        # Internal Function:
        refresh.code = function(...)
        {
            # Sliders:
            mean = .sliderMenu(no = 1)
            std  = .sliderMenu(no = 2)
            
            # Plot Data:      
            xmin = round(qnorm(0.01, mean, std), digits = 2)
            xmax = round(qnorm(0.99, mean, std), digits = 2)
            x = seq(xmin, xmax, length = 100)
            y = dnorm(x, mean, std)
            main = paste("Normal Density\n", 
                "mean = ", as.character(mean), " | mu = ", as.character(std))       
            par(mfrow = c(1, 1))
            
            # Density:
            plot(x, y, type = "l", xlim = c(xmin, xmax), col = "steelblue")
            grid()
            title(main = main)     
        }
      
        # Open Slider Menu:
        .sliderMenu(refresh.code,
           names =       c( "mean",  "std"),
           minima =      c(  -5.00,    0.1),
           maxima =      c(   5.00,   10.0),
           resolutions = c(   0.10,    0.1),
           starts =      c(   0.00,    1.0))
    }
    
    # Try:
    .normalSlider()

    
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.someUtilities = 
function()
{
    #  .description              Sets default description string
    #  .unirootNA                Computes zero without error exit
    #  .datax                    Loads timeSeries objects from demo files   
    
    # Description used by S4 Classes:
    .description()
    
    # Compute zero:
    # uniroot(sin, c(1,2))
    #   Error in uniroot(sin, c(1, 2)) : 
    #       f() values at end points not of opposite sign
    # Now try:
    .unirootNA(sin, c(1, 2)) 
    # Try:
    .unirootNA(sin, c(-1, 1))
    
    # datax
    
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.dateFunctions = 
function()
{    
    #  .fjulian                  Transform formatted dates to julian day numbers    
    #  .julian                   Implements SPlus like 'julian'                                                              
    #  .isISO8601                Checks if the date/time is ISO8601 formatted                                               
    #  .isPOSIX                  Checks for an object of class POSIX                                                        
    #  .by2seconds               Converts 'by' string into numeric value of seconds    
    
    # Transform formatted dates to julian day numbers - .fjulian()
    require(date)
    fdates = c("8/11/73", "08-11-73", "August 11 1973", "Aug11/73")
    .fjulian(fdates) 
    fdates = c("11/8/73", "11-08-73", "11 August 1973", "11Aug73")
    .fjulian(fdates, order = 'dmy')
    
    # Implement SPlus like 'julian'  - .julian()
    # .julian(m, d, y, origin = c(month = 1, day = 1, year = 1960))
    .julian(1, 1, 1970)
    
    # Check if the date/time is ISO8601 formatted 
    # .isISO8601(c("2007-01-01", "2007-12-31" ))
    # .isISO8601(c("2007-01-01", "2007-12-31" ))[[1]]
    # .isISO8601("2007-Jan-01")[[1]]
    # .isISO8601("2007-01-01 15:00:000")[[1]]
    # ... should go to fCalendar 
    # Check for an object of class POSIX 
    class(date())
    .isPOSIX(date())
    class(Sys.time())
    .isPOSIX(Sys.time())
    
    # Convert 'by' string into numeric value of seconds 
    .by2seconds("2 h")
    .by2seconds("30 m")
    
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.someMethods = 
function()
{    
    #  .print                    Internal print method                              
    #  .plot                     Internal plot method                               
    #  .summary                  Internal summary method                            
    #  .predict                  Internal predict method    
    
    # Return Value:
    return() 
}


################################################################################

