
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
# FUNCTION:             DESCRIPTION:    
#  tsPlot                Returns a time series plot
#  histPlot              Returns a histogram plot
#  densityPlot           Returns a kernel density estimate plot
#  .splusLikePlot        Sets parameters that plots look more Splus like
# FUNCTION:             3-DIMENSIONAL PLOTS:
#  circlesPlot           Returns a scatterplot of circles indexing 3rd variable
#  perspPlot             Returns a perspective plot in 2 dimensions
#  contourPlot           Returns a contour plot in 2 dimensions
# FUNCTION:             TABLES AND PALETTES:
#  characterTable        Shows a table of character's numerical equivalents 
#  plotcharacterTable    Shows a table of plot characters and symbols
#  colorTable            Shows a table of plot color codes
#  .chcode               Changes from one to another number system
#  .hex.to.dec           Converts heximal numbers do decimal numbers
#  .dec.to.hex           Converts decimal numbers do heximal numbers
#  grey.pal              Creates a grey palette like rainbow does for colors
# FUNCTION:             SLIDER MENU:
#  .sliderMenu           Starts a slider menu
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PlotFunctions); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

    
test.plotSeries =
function()
{   
    # fCalendar
    require(fCalendar)

    # tsPlot -
    data(DowJones30)
    DowJones.ts = as.timeSeries(DowJones30)[, c("CAT", "GE", "IBM", "JPM", )]
    par(mfrow = c(1, 1))
    tsPlot(DowJones.ts)
    title(main = "CAT - GE - IBM - JPM")
    
    # histPlot -
    DowJones.ret = returnSeries(DowJones.ts)
    par(mfrow = c(2, 2), cex = 0.7)
    histPlot(x = DowJones.ret)

    # densityPlot -
    par(mfrow = c(2, 2), cex = 0.7)
    densityPlot(x = DowJones.ret)
    
    # Return Value:
    return()
}
   
    
# ------------------------------------------------------------------------------
    

test.plotUtils =
function()
{     
    # circlesPlot -
    par(mfrow = c(1, 1), cex = 0.7)
    circlesPlot(x = rnorm(50), y = rnorm(50), size = abs(rnorm(50)),
        main = "Circles Plot")
    circlesPlot(x = rnorm(50), y = rnorm(50), size = 1,
        main = "Circles Plot")
    
    # perspPlot -
    par(mfrow = c(1, 1))
    x = y = seq(-10, 10, length = 51)
    f = function(x, y) { r = sqrt(x^2+y^2); 10 * sin(r)/r }
    z = outer(x, y, f)   
    perspPlot(x, y, z)
    title(main = "Perspective Plot", line = -1)
    
    # contourPlot -
    par(mfrow = c(1, 1))
    x = y = seq(-10, 10, length = 51)
    f = function(x, y) { r = sqrt(x^2+y^2); 10 * sin(r)/r }
    z = outer(x, y, f)   
    contourPlot(x, y, z)
    title(main = "Contour Plot", line = -1)
    
    # characterTable - 
    cat("\n", "\251 \n")
    cat("\n", characterTable(5), "\n\n")
    
    # colorTable - 
    colorTable()
    
    # plotcharacter Table - 
    plotcharacterTable()
    
    # Return Value:
    return()
    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fBasics/test/runit012B.R")
    printTextProtocol(testResult)
}


################################################################################

