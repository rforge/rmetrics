
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

        
.fSeries.PopupMenu =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fBasics Popup Menu
    
    # FUNCTION:
    
    # Menu:
    fSeriesMenu <<- newToolbarMenu()
    
    
    # Add Menu:
    Label = "Arma Modelling"
    subLabel = c(
        "* Example timeSeries: x = NYSE Returns",
        "* Example vector: x = GBPDEM Returns",
        "Print fARMA Class Representation", 
        "ARMA Series Simulation", 
        "ARMA Parameter Estimation",
        "... Print Summary Report",
        "... Fitted Values",
        "... Residual Values",
        "Forecast ARMA Process")
     Command = c(
        ".fSeries.ArmaModelling.1",
        ".fSeries.ArmaModelling.2",
        ".fSeries.ArmaModelling.3",
        ".fSeries.ArmaModelling.4",
        ".fSeries.ArmaModelling.5",
        ".fSeries.ArmaModelling.6",
        ".fSeries.ArmaModelling.7",
        ".fSeries.ArmaModelling.8",
        ".fSeries.ArmaModelling.9")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    
    # Add Menu:
    Label = "Garch Modelling"
    subLabel = c(
        "* Example timeSeries: x = NYSE Returns",
        "* Example vector: x = GBPDEM Returns",
        "Print fGARCH Class Representation",
        "Simulate GARCH Process",
        "Fit GARCH Process",
        "... Print Summary Report",
        "... Fitted Values",
        "... Residual Values",
        "Print APARCH Class Representation",
        "Simulate APARCH Process",
        "Fit APARCH Process",
        "... Print Summary Report",
        "... Fitted Values",
        "... Residual Values")
    Command = c(
        ".fSeries.GarchModelling.1",
        ".fSeries.GarchModelling.2",
        ".fSeries.GarchModelling.3",
        ".fSeries.GarchModelling.4",
        ".fSeries.GarchModelling.5",
        ".fSeries.GarchModelling.6",
        ".fSeries.GarchModelling.7",
        ".fSeries.GarchModelling.8",
        ".fSeries.GarchModelling.9",
        ".fSeries.GarchModelling.10",
        ".fSeries.GarchModelling.11",
        ".fSeries.GarchModelling.12",
        ".fSeries.GarchModelling.13",
        ".fSeries.GarchModelling.14")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    
    # Add Menu:
    Label = "Long Memory Modelling"
    subLabel = c(
        "FGN Series Simulation")
    Command = c(
        ".fSeries.LongMemoryModelling.1")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    
    # Add Menu:
    Label = "Chaotic Time Series"
    subLabel = c(
        "Henon Map", 
        "Ikeda Map", 
        "Logistic Map", 
        "Lorentz Attractor", 
        "Roessler Attractor")
    Command = c(
        ".fSeries.ChaoticTimeSeries.1",
        ".fSeries.ChaoticTimeSeries.2",
        ".fSeries.ChaoticTimeSeries.3",
        ".fSeries.ChaoticTimeSeries.4",
        ".fSeries.ChaoticTimeSeries.5")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
        
    
    # Add Menu:
    Label = "Portable Innovations"
    subLabel = c(
        "Portable Uniform Innovations", 
        "Portable Normal Innovations", 
        "Portable Student-t Innovations")
    Command = c(
        ".fSeries.PortableInnovations.1",
        ".fSeries.PortableInnovations.2",
        ".fSeries.PortableInnovations.3")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
      
    
    # Add Menu:
    Label = "Time Series Tests"
    subLabel = c(
        # runs Test goes here ?
        "* Example timeSeries: x - NYSE log Returns",
        "BDS Test", 
        "Teraesvirta NN Test", 
        "White NN Test")
    Command = c(
        ".fSeries.TimeSeriesTests.1",
        ".fSeries.TimeSeriesTests.2",
        ".fSeries.TimeSeriesTests.3",
        ".fSeries.TimeSeriesTests.4")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
        
    
    # Add Menu:
    Label = "Unit Root Distribution"
    subLabel = c(
        #  punitroot - Returns McKinnon's cumulative probability
        #  qunitroot - Returns McKinnon's quantiles
        "Unitroot Distribution")
    Command = c(
        ".fSeries.UnitRootDistribution.1")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
        
    
    # Add Menu:
    Label = "Unit Root Tests"
    subLabel = c(
        "* Example timeSeries: x - has Unit Root",
        "* Example timeSeries: x - has no Unit Root",
        # "Print fURTEST Class Representation"
        # "is.fURTEST Object ?"
        "ADF Test",
        "McKinnon's Unit Root Test")
        #
        #  urersTest - Elliott-Rothenberg-Stock test for unit roots
        #  urkpssTest - KPSS unit root test for stationarity
        #  urppTest - Phillips-Perron test for unit roots
        #  urspTest - Schmidt-Phillips test for unit roots
        #  urzaTest - Zivot-Andrews test for unit roots
        # "Summary Report"
    Command = c(
        ".fSeries.UnitRootTests.1",
        ".fSeries.UnitRootTests.2",
        ".fSeries.UnitRootTests.3",
        ".fSeries.UnitRootTests.4")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
        
    
    # Add Menu:
    Label = "Heaviside Function"
    subLabel = c(
        "Heaviside And Related Functions")
    Command = c(
        ".fSeries.HeavisideFunction.1")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
        
    
    # Add Menu:
    Label = "Garch Distributions"
    subLabel = c(
        "Generate Skew Normal Random Numbers",
        "Skew Normal Distribution Slider",
        "Generate Skew Student-t Random Numbers",
        "Skew Student-t Distribution Slider",
        "Generate Skew GED Random Numbers",
        "Skew GED Distribution Slider")
    Command = c(
        ".fSeries.GarchDistributions.1",
        ".fSeries.GarchDistributions.2",
        ".fSeries.GarchDistributions.3",
        ".fSeries.GarchDistributions.4",
        ".fSeries.GarchDistributions.5",
        ".fSeries.GarchDistributions.6")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
        
   
    # Add Menu:
    Label = "Garch Distribution Fits"
    subLabel = c(
        "* Example timeSeries: x - NYSE log Returns",
        "Fit Normal Distribution",
        "Fit Skew Normal Distribution",
        "Fit Sudent-t Distribution",
        "Fit Skew Sudent-t Distribution",
        "Fit GED Distribution",
        "Fit Skew GED Distribution")
    Command = c(
        ".fSeries.GarchDistributionFits.1",
        ".fSeries.GarchDistributionFits.2",
        ".fSeries.GarchDistributionFits.3",
        ".fSeries.GarchDistributionFits.4",
        ".fSeries.GarchDistributionFits.5",
        ".fSeries.GarchDistributionFits.6",
        ".fSeries.GarchDistributionFits.7")   
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Series Data"
    subLabel = c(
        "nyseres - ",
        "recession - ",
        "dem2gbp - ",
        "cac40 - ", 
        "nelsonplosser - ", 
        "surex1.ts - ",
        "rf.30day - ",
        "black.ts - ",
        "klein - ",
        "kmenta - ")
    Command = c(
        ".fSeries.SeriesData.1",
        ".fSeries.SeriesData.2",
        ".fSeries.SeriesData.3",
        ".fSeries.SeriesData.4",
        ".fSeries.SeriesData.5",
        ".fSeries.SeriesData.6",
        ".fSeries.SeriesData.7",
        ".fSeries.SeriesData.8",
        ".fSeries.SeriesData.9",
        ".fSeries.SeriesData.10",)
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
        
    # Add Menu: 
    Label = "Mills Data"
    subLabel = c(
        "RS - Monthly 91 Day Treasury Bill Rate",  
        "R20 - Monthly Yield on 20 Year UK Gilts",  
        "RSQ - Quarterly 91 Day Treasury Bill Rate",  
        "R20Q - Quarterly Yield on 20 Year UK Gilts", 
        "RSQREAL - Quarterly Real 91 Day Treasury Bill",  
        "FTAPRICE - FTA All Share Price Index",  
        "FTADIV - FTA All Share Dividend Index",  
        "FTARET - FTA All Share Nominal Returns",  
        "RPI - UK Retail Price Index",  
        "EXCHD - Dollar/Sterling Exchange Rate",  
        "EXCHQ - Dollar/Sterling Exchange Rate",  
        "SP500 - SP 500 Annual Data Index",  
        "SP500R - SP 500 Real Returns",  
        "SP500D - SP 500 Daily Data Index",  
        "FT30 - Financial Times FT 30 Index",  
        "FTSE100 - FTSE 100 Index",  
        "CTLD - Courtaulds Share Price",  
        "LGEN - Legal and General Share Price",  
        "PRU - Prudential Share Price")
    Command = c(
        ".fSeries.MillsData.1",
        ".fSeries.MillsData.2",
        ".fSeries.MillsData.3",
        ".fSeries.MillsData.4",
        ".fSeries.MillsData.5",
        ".fSeries.MillsData.6",
        ".fSeries.MillsData.7",
        ".fSeries.MillsData.8",
        ".fSeries.MillsData.9",
        ".fSeries.MillsData.10",
        ".fSeries.MillsData.11",
        ".fSeries.MillsData.12",
        ".fSeries.MillsData.13",
        ".fSeries.MillsData.14",
        ".fSeries.MillsData.15",
        ".fSeries.MillsData.16",
        ".fSeries.MillsData.17",
        ".fSeries.MillsData.18",
        ".fSeries.MillsData.19")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fSeriesMenu, Label = "fSeries")
        
}


################################################################################

