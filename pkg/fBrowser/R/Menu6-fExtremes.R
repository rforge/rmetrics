
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file 
    
    
################################################################################
# fExtremes Popup

        
.fExtremes.PopupMenu =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Menu:
    fExtremesMenu <<- newToolbarMenu()
        
    # Add Menu:
    Label = "Extremes Plots"
    subLabel = c(
        "* Example timeSeries: x = BMW Daily Returns",
        "Plot of Empirical Distribution",
        "Plot of Normal Quantiles",
        "Plot of Pareto Quantiles",
        "Sample Mean Excess Plot",
        "Mean Excess Function Plot",
        "Mean Residual Life Plot",
        "Plot of Max/Sum Ratio",
        "Development of Records",
        "Plot of ACF of Exceedences")
    Command = c(
        ".fData.bmwDaily",
        ".fExtremes.ExtremesPlots.emd",
        ".fExtremes.ExtremesPlots.qq",
        ".fExtremes.ExtremesPlots.q",
        ".fExtremes.ExtremesPlots.me",
        ".fExtremes.ExtremesPlots.mrl",
        ".fExtremes.ExtremesPlots.mxf",
        ".fExtremes.ExtremesPlots.msratio",
        ".fExtremes.ExtremesPlots.records",
        ".fExtremes.ExtremesPlots.xacf")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Data Preprocessing"
    subLabel = c(
        "* Example timeSeries: x = BMW Daily Returns",
        "Find Threshold")
    Command = c(
        ".fData.bmwDaily",
        ".fExtremes.DataPreprocessing.findThreshold")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Block Maxima"
    subLabel = c(
        "* Example timeSeries: x = BMW Daily Returns",
        "GEV Distribution",
        "GEV Simulation", 
        "Generate Block Maxima Series",
        "Generate Block Maxima Vector",
        "GEV Parameter Estimation",
        "Return Level Plot",
        "Hill Plot", 
        "Shape Parameter Plots")
    Command = c(
        ".fData.bmwDaily",
        ".fExtremes.GEV.gevSlider",
        ".fExtremes.GEV.sim",
        ".fExtremes.GEV.blockmaxSeries",
        ".fExtremes.GEV.blockmaxVector",
        ".fExtremes.GEV.fit",
        ".fExtremes.GEV.gevrlevelPlot",
        ".fExtremes.MDA.hillPlot",
        ".fExtremes.MDA.shaparmPlot") 
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)        

    # Add Menu:
    Label = "Peaks Over Threshold"
    subLabel = c(
        "* Example timeSeries: x = BMW Daily Returns",
        "GPD Distribution",
        "GPD Simulation", 
        "GPD Parameter Estimation")
    Command = c(
        ".fData.bmwDaily",
        ".fExtremes.GPD.gpdSlider",
        ".fExtremes.GPD.sim",
        ".fExtremes.GPD.fit")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
                     
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fExtremesMenu, Label = "fExtremes")
           
}


################################################################################

