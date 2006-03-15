
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
# fSeries Popup Menu

        
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
        "* Demo tS: x = NYSE Returns",
        "_______________________________",
        "ARMA Series Simulation", 
        "ARMA Parameter Estimation",
        "ARMA Forecasting")
     Command = c(
        ".fData.nyseDaily",
        "tkSeparator",
        ".fSeries.ArmaModelling.armaSim",
        ".fSeries.ArmaModelling.armaFit",
        ".fSeries.ArmaModelling.predict")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    
    # Add Menu:
    Label = "Garch Modelling"
    subLabel = c(
        "* Demo tS: x = DEMGBP Daily Returns",
        "_____________________________________",
        "GARCH Series Simulation", 
        "GARCH Parameter Estimation",
        "GARCH Forecasting",
        "_____________________________________",
        "Conditional Distribution Slider",
        "Conditional Distribution Fit")
    Command = c(
        ".fData.dem2gbpDaily",
        "tkSeparator",
        ".fSeries.GarchModelling.garchSim",
        ".fSeries.GarchModelling.garchFit",
        ".fSeries.GarchModelling.predict",
        "tkSeparator",
        ".fSeries.GarchDistributions.conddistSlider",
        ".fSeries.GarchDistributions.conddistFit")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
          
    # Add Menu:
    Label = "________________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command) 
      
    if (FALSE) {
    # Add Menu:
    Label = "Long Memory Modelling"
    subLabel = c(
        "Simulate FGN Process",
        "Simulate FBM/FGN Process")
    Command = c(
        ".fSeries.LongMemoryModelling.fgnSim",
        ".fSeries.LongMemoryModelling.fbmSim")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    }
    
    # Add Menu:
    Label = "Dependency Tests"
    subLabel = c(
        # runs Test goes here ?
        "* Demo tS: x - NYSE Daily Returns",
        "_________________________________",
        "BDS Test", 
        "Teraesvirta NN Test", 
        "White NN Test")
    Command = c(
        ".fData.nyseDaily",
        "tkSeparator",
        ".fSeries.TimeSeriesTests.bdsTest",
        ".fSeries.TimeSeriesTests.tnnTest",
        ".fSeries.TimeSeriesTests.wnnTest")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
        

    # Add Menu:
    Label = "Unit Root Tests"
    subLabel = c(
        "* Demo tS: x - has Unit Root",
        "* Demo tS: x - has no Unit Root",
        "___________________________________",
        "Augmented Dickey-Fuller Test",
        "McKinnon's ADF Test",
        "______________________________[urca]",
        "Elliott-Rothenberg-Stock Test",
        "KPSS unit root test for stationarity",
        "Phillips-Perron test for unit roots",
        "Schmidt-Phillips test for unit roots",
        "Zivot-Andrews test for unit roots")
    Command = c(
        ".fSeries.UnitRootTests.hasUnitRoot",
        ".fSeries.UnitRootTests.hasNoUnitRoot",
        "tkSeparator",
        ".fSeries.UnitRootTests.adfTest",
        ".fSeries.UnitRootTests.unitrootTest",
        "tkSeparator",
        ".fSeries.UnitRootTests.urersTest",
        ".fSeries.UnitRootTests.urkpssTest",
        ".fSeries.UnitRootTests.urppTest",
        ".fSeries.UnitRootTests.urspTest",
        ".fSeries.UnitRootTests.urzaTest")
    addToolbarMenu(fSeriesMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "________________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
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

