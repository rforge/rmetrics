
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
#   1999 - 2006, Diethelm Wuertz, GPL
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


test.helpFile = 
function()
{
    # Help File:
    # helpFile = function() { 
    #     example(TimeSeriesData, ask = FALSE)
    #     return() 
    # }
    # checkIdentical(
    #     target = class(try(helpFile())),
    #     current = "NULL")
    #     
    # Return Value:
    # return() 
}


# ------------------------------------------------------------------------------


test.plotFunctions = 
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


test.bivariatePlots = 
function()
{
    #  .responsesPlot            Returns a response series plot
    #  .firePlot                 Returns a fitted values vs.residuals plot
    
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.3dPlots = 
function()
{
    #  .circlesPlot              Returns a circles plot indexing 3rd variable
    #  .perspPlot                Returns a perspective plot in 2 dimensions
    #  .contourPlot              Returns a contour plot in 2 dimensions
    #  .histStack                Returns a stacked histogram plot
    
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.sliderMenu = 
function()
{
    #  .sliderMenu               Starts a slider menu   
    
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


# ------------------------------------------------------------------------------        
        
       
if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fEcofin/test/runit0A.R")
    printTextProtocol(testResult)
}   


################################################################################

