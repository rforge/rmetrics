
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
        "* Example timeSeries: x = NYSE Returns",
        "Plot of Empirical Distribution",
        "Plot of Normal Quantiles",
        "... with 95% Intervals",
        "Plot of Pareto Quantiles",
        "Sample Mean Excess Plot",
        "... Mean Excess Function Plot",
        "... Mean Residual Life Plot",
        "Plot of Max/Sum Ratio",
        "Records Development",
        "... Development of Subsamples",
        "Plot of ACF of Exceedences")
    Command = c(
        ".fExtremes.Plots.bmw",
        ".fExtremes.Plots.emd",
        ".fExtremes.Plots.qq",
        ".fExtremes.Plots.qqbayes",
        ".fExtremes.Plots.q",
        ".fExtremes.Plots.me",
        ".fExtremes.Plots.mrl",
        ".fExtremes.Plots.mxf",
        ".fExtremes.Plots.msratio",
        ".fExtremes.Plots.records",
        ".fExtremes.Plots.ssrecords",
        ".fExtremes.Plots.xacf")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Generalized Extreme Values"
    subLabel = c(
        "* Example timeSeries: x = BMW Max Losses",
        "GEV Simulation", 
        "... Random Number Slider",
        "... Distribution Slider",
        "GEV Parameter Estimation",
        "... Summary Report",
        "... Return Level Plot",
        "Hill Plot", 
        "Shape Parameter Plots")
    Command = c(
        ".fExtremes.GEV.bmw",
        ".fExtremes.GEV.sim",
        ".fExtremes.GEV.RandSlider",
        ".fExtremes.GEV.DistSlider",
        ".fExtremes.GEV.fit",
        ".fExtremes.GEV.summary",
        ".fExtremes.GEV.rlevel",
        ".fExtremes.MDA.hill",
        ".fExtremes.MDA.shaparm") 
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)        

    # Add Menu:
    Label = "Peaks Over Threshold"
    subLabel = c(
        "* Example timeSeries: x = BMW Max Losses",
        "GPD Simulation", 
        "... Random Number Slider",
        "... Distribution Slider",
        "GPD Parameter Estimation",
        "... Summary Report")
    Command = c(
        ".fExtremes.GPD.bmw",
        ".fExtremes.GPD.sim",
        ".fExtremes.GPD.RandSlider",
        ".fExtremes.GPD.DistSlider",
        ".fExtremes.GPD.fit",
        ".fExtremes.GPD.summary")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
                     
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fExtremesMenu, Label = "fExtremes")
           
}


################################################################################

