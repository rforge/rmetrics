
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
# fSSeries Popup Menu

        
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
        "Print fARMA Class Representation", 
        "ARMA Series Simulation", 
        "ARMA Parameter Estimation",
        "Forecast ARMA Process")
     Command = c(
        ".fSeries.ArmaModelling.nyseDaily",
        ".fSeries.ArmaModelling.fARMA",
        ".fSeries.ArmaModelling.armaSim",
        ".fSeries.ArmaModelling.armaFit",
        ".fSeries.ArmaModelling.predict")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    
    # Add Menu:
    Label = "Garch Modelling"
    subLabel = c(
        "* Example timeSeries: x = DEMGBP Returns",
        "Print fGARCH Class Representation", 
        "GARCH Series Simulation", 
        "GARCH Parameter Estimation",
        "Forecast GARCH Process")
    Command = c(
        ".fSeries.GarchModelling.dem2gbpDaily",
        ".fSeries.GarchModelling.fGARCH",
        ".fSeries.GarchModelling.garchSim",
        ".fSeries.GarchModelling.garchFit",
        ".fSeries.GarchModelling.predict")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    
    if (FALSE) {
    # Add Menu:
    Label = "Long Memory Modelling"
    subLabel = c(
        "Print fHURST Class Representation",
        "Simulate FGN Process",
        "Simulate FBM/FGN Process")
    Command = c(
        ".fSeries.LongMemoryModelling.fHURST",
        ".fSeries.LongMemoryModelling.fgnSim",
        ".fSeries.LongMemoryModelling.fbmSim")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    }
    
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
    Label = "Unit Root Tests"
    subLabel = c(
        "* Example timeSeries: x - has Unit Root",
        "* Example timeSeries: x - has no Unit Root",
        # "Print fURTEST Class Representation"
        # "is.fURTEST Object ?"
        "ADF Test",
        "McKinnon's Unit Root Test",
        "Unitroot Distribution")
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
        ".fSeries.UnitRootTests.4",
        ".fSeries.UnitRootDistribution.1")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
       
    
    # Add Menu:
    Label = "Garch Distributions"
    subLabel = c(
        "Generate Skew Normal Random Numbers",
        "... Skew Normal Distribution Slider",
        "Generate Skew Student-t Random Numbers",
        "... Skew Student-t Distribution Slider",
        "Generate Skew GED Random Numbers",
        "... Skew GED Distribution Slider",
        "Heaviside And Related Functions")
    Command = c(
        ".fSeries.GarchDistributions.1",
        ".fSeries.GarchDistributions.2",
        ".fSeries.GarchDistributions.3",
        ".fSeries.GarchDistributions.4",
        ".fSeries.GarchDistributions.5",
        ".fSeries.GarchDistributions.6",
        ".fSeries.HeavisideFunction.1")
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
    Label = "DATA: fSeries Time Series"
    subLabel = c(
        "nyseres - NYSE Residuals",
        "recession - Recession Data",
        "dem2gbp - DEM GBP FX Rate",
        "cac40 - CAC 40 Index", 
        "nelsonplosser - Nelson Plosser", 
        "surex1.ts - Spot Returns and Forward Premiums",
        "rf.30day - Interest Rate on 30-day US Tbills",
        "black.ts - Monthly Srock Returns from Berndt",
        "klein - Klein's US Economy Model Data",
        "kmenta - Kmenta's SUR Model Data Set")
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
    Label = "DATA: Portable Innovations"
    subLabel = c(
        "Portable Uniform Innovations", 
        "Portable Normal Innovations", 
        "Portable Student-t Innovations")
    Command = c(
        ".fSeries.PortableInnovations.runiflcg",
        ".fSeries.PortableInnovations.rnormlcg",
        ".fSeries.PortableInnovations.rtlcg")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "DATA: Chaotic Time Series Maps"
    subLabel = c(
        "Simulate Henon Map", 
        "Simulate Ikeda Map", 
        "Simulate Logistic Map", 
        "Simulate Lorentz Attractor", 
        "Simulate Roessler Attractor")
    Command = c(
        ".fSeries.ChaoticTimeSeries.henonSim",
        ".fSeries.ChaoticTimeSeries.ikedaSim",
        ".fSeries.ChaoticTimeSeries.logisticSim",
        ".fSeries.ChaoticTimeSeries.lorentzSim",
        ".fSeries.ChaoticTimeSeries.roesslerSim")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
        
        
    # Add Menu: 
    Label = "DATA: Mills Textbook"
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
        ".fSeries.MillsData.RS",
        ".fSeries.MillsData.R20",
        ".fSeries.MillsData.RSQ",
        ".fSeries.MillsData.R20Q",
        ".fSeries.MillsData.RSQREAL",
        ".fSeries.MillsData.FTAPRICE",
        ".fSeries.MillsData.FTADIV",
        ".fSeries.MillsData.FTARET",
        ".fSeries.MillsData.RPI",
        ".fSeries.MillsData.EXCHD",
        ".fSeries.MillsData.EXCHQ",
        ".fSeries.MillsData.SP500",
        ".fSeries.MillsData.SP500R",
        ".fSeries.MillsData.SP500D",
        ".fSeries.MillsData.FT30",
        ".fSeries.MillsData.FTSE100",
        ".fSeries.MillsData.CTLD",
        ".fSeries.MillsData.LGEN",
        ".fSeries.MillsData.PRU")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fSeriesMenu, Label = "fSeries")
        
}


################################################################################

