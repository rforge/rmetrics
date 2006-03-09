
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
# FUNCTION:
# .SeriesPopup
################################################################################

        
.fExtremes.PopupMenu =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fBasics Popup Menu
    
    # FUNCTION:
    
    # Menu:
    fExtremesMenu <<- newToolbarMenu()
        
    
    # Add Menu:
    Label = "Extremes Plots"
    subLabel = c(
        "* Example timeSeries: x = NYSE Returns",
        "Empirical Distribution Function",
        "Quantile Quantile Plot",
        "QQ-Plot with 95% Intervals",
        #Exploratory QQ Plot for EV Analysis",
        "Sample Mean Excess Plot",
        "... Mean Excess Function Plot",
        "... Mean Residual Life Plot",
        "Records Development",
        "... Development of Subsamples",
        "Ratio of Maximums and Sums",
        "ACF Exceedences of Heights",
        "... of Distances")
    Command = c(
        ".fExtremes.A2Cmd.1",
        ".fExtremes.A2Cmd.2",
        ".fExtremes.A2Cmd.3",
        ".fExtremes.A2Cmd.4",
        ".fExtremes.A2Cmd.5",
        ".fExtremes.A2Cmd.6",
        ".fExtremes.A2Cmd.7",
        ".fExtremes.A2Cmd.8",
        ".fExtremes.A2Cmd.9",
        ".fExtremes.A2Cmd.10",
        ".fExtremes.A2Cmd.11",
        ".fExtremes.A2Cmd.12")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
              
    # Add Menu:
    Label = "Extremes Preprocessing"
    subLabel = c(
        "* Example timeSeries: x = NYSE Returns",
        "5% Series Points below Threshold Value",
        "5% Series Points above Threshold Value",
        "Monthly Block Minima Series Points",
        "Monthly Block Maxima Series Points")
    Command = c(
        ".fExtremes.A3Cmd.1",
        ".fExtremes.A3Cmd.2",
        ".fExtremes.A3Cmd.3",
        ".fExtremes.A3Cmd.4",
        ".fExtremes.A3Cmd.5")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
        
    # Add Menu:
    Label = "Gev Distribution"
    subLabel = c(
        "Generate GEV Random Numbers",
        "GEV Distribution Slider")
    Command = c(
        ".fExtremes.B1Cmd.1",
        ".fExtremes.B1Cmd.2")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
        
    # Add Menu:
    Label = "Gev Fit"
    subLabel = c(
        "* Example timeSeries: x = BMW Max Losses",
        "GEV Simulation", 
        "GEV Parameter Estimation")
    Command = c(
        ".fExtremes.B2Cmd.1",
        ".fExtremes.B2Cmd.2",
        ".fExtremes.B2Cmd.3")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
                  
    # Add Menu:
    Label = "Gev Glm Fit"
    subLabel = c(
        "NYI: GEV GLM Fit")
    Command = c(
        ".fExtremes.B3Cmd.1")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
               
    # Add Menu:
    Label = "Mda Plots"
    subLabel = c(
        "* Example timeSeries: x = BMW Log Returns",
        "Hill Plot", 
        "Shape Parameter Plots")
    Command = c(
        ".fExtremes.B4Cmd.1",
        ".fExtremes.B4Cmd.2",
        ".fExtremes.B4Cmd.3")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
                  
    # Add Menu:
    Label = "Gpd Distribution"
    subLabel = c(
        "Generate Random Numbers",
        "GPD Distribution Slider")
    Command = c(
        ".fExtremes.C1Cmd.1",
        ".fExtremes.C1Cmd.2")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    

    # Add Menu:   
    Label = "Gpd Fit"
    subLabel = c(
        "* Example timeSeries: x = Danish Fire Losses",
        "GPD Simulation",
        "GPD Fit")
    Command = c(
        ".fExtremes.C2Cmd.1",
        ".fExtremes.C2Cmd.2",
        ".fExtremes.C2Cmd.3")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
           
           
    # Add Menu:
    Label = "Gpd Glm Fit"
    subLabel = c(
        "NYI: GPD GLM Fit")
    Command = c(
        ".fExtremes.C3Cmd.1")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
                  
    # Add Menu:
    Label = "Pot Fit"
    subLabel = c(
        "* Example timeSeries: x = Danish Fire Losses",
        "POT Simulation",
        "POT Fit",
        "... Summary Report",
        "k-Block Return Level")
    Command = c(
        ".fExtremes.C4Cmd.1",
        ".fExtremes.C4Cmd.2",
        ".fExtremes.C4Cmd.3",
        ".fExtremes.C4Cmd.4",
        ".fExtremes.C4Cmd.5")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
        
    # Add Menu:
    Label = "PP Fit"
    subLabel = c(
        "PP Fit")
    Command = c(
        ".fExtremes.C5Cmd.1")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
        
    # Add Menu:
    Label = "Rlarg Fit"
    subLabel = c(
        "Rlarg Fit")
    Command = c(
        ".fExtremes.C6Cmd.1")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
        
    # Add Menu:
    Label = "Extremal Index Plots"
    subLabel = c(
        "* Example timeSeries: x = BMW log Returns",
        "Monthly Extremal Index - Lower Tail", 
        "N-Days Extremal Index - Lower Tail",
        "Monthly Extremal Index - Upper Tail", 
        "N-Days Extremal Index - Lower Tail")
    Command = c(
        ".fExtremes.D1Cmd.1",
        ".fExtremes.D1Cmd.2",
        ".fExtremes.D1Cmd.3",
        ".fExtremes.D1Cmd.4",
        ".fExtremes.D1Cmd.5")
    addToolbarMenu(fExtremesMenu, Label, subLabel, Command)
    
                  
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fExtremesMenu, Label = "fExtremes")
           
}


################################################################################

