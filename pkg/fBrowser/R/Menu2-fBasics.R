
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
#  .fBasics.PopupMenu
################################################################################


.fBasics.PopupMenu =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fBasics Popup Menu
    
    # FUNCTION:
    
    # Menu:
    fBasicsMenu <<- newToolbarMenu()
     
    # Add Menu:
    Label = "Web Data Import"
    subLabel = c(
        "Print fWEBDATA Class representation",
        "Economagic Series Download", 
        "Yahoo Series Download",
        "FRED St. Louis Series Download",
        "... Extract Data Slot as.timeSeries",
        "Yahoo Key Statistics Download")
    Command = c(
        ".fBasics.TimeSeriesImport.1",
        ".fBasics.TimeSeriesImport.2",
        ".fBasics.TimeSeriesImport.3",
        ".fBasics.TimeSeriesImport.4",
        ".fBasics.TimeSeriesImport.5",
        ".fBasics.TimeSeriesImport.6")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Basic Statistics"
    subLabel = c(
        "* Example timeSeries: x = SP500 Returns",
        "mean - Compute Mean",
        "var - Compute Variance",
        "skewness - Compute Skewness",
        "kurtosis - Compute Kurtosis",
        "summary - Summary Report",
        "basicStats - Basic Statistics")
    Command = c(
        ".fBasics.BasicStatistics.1",
        ".fBasics.BasicStatistics.2",
        ".fBasics.BasicStatistics.3",
        ".fBasics.BasicStatistics.4",
        ".fBasics.BasicStatistics.5",
        ".fBasics.BasicStatistics.6",
        ".fBasics.BasicStatistics.7")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Basic Plots"
    subLabel = c(
        "* Example timeSeries: x = SP500 Returns",
        "1D: ACF Plot",
        "1D: PACF Plot",
        "1D: Series Plot", 
        "1D: Histogram Plot",
        "* Example timeSeries: x = MSFT|SP500 Returns",
        "2D: Series Plot",
        "2D: Scatterdiagramm Plot")
    Command = c(
        ".fBasics.PlotFunctions.1",
        ".fBasics.PlotFunctions.2",
        ".fBasics.PlotFunctions.3",
        ".fBasics.PlotFunctions.4",
        ".fBasics.PlotFunctions.5",
        ".fBasics.PlotFunctions.6",
        ".fBasics.PlotFunctions.7",
        ".fBasics.PlotFunctions.8")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Return Distributions"
    subLabel = c(
        "Generate NORM Random Numbers",
        "Plot NORM Distribution",
        "Generate HYP Random Numbers",
        "Plot HYP Distribution", 
        "Generate NIG Random Numbers",
        "Plot NIG Distribution",
        "Generate SYMSTB Random Numbers",
        "Plot SYMSTB Distribution", 
        "Generate STABLE Random Numbers",
        "Plot STABLE Distribution",
        "* Example timeSeries: x - SP500 Returns",
        "NYI - Smoothed Spline Density Plot")
    Command = c(
        ".fBasics.ReturnDistributions.1",
        ".fBasics.ReturnDistributions.2",
        ".fBasics.ReturnDistributions.3",
        ".fBasics.ReturnDistributions.4",
        ".fBasics.ReturnDistributions.5",
        ".fBasics.ReturnDistributions.6",
        ".fBasics.ReturnDistributions.7",
        ".fBasics.ReturnDistributions.8",
        ".fBasics.ReturnDistributions.9",
        ".fBasics.ReturnDistributions.10",
        ".fBasics.ReturnDistributions.11",
        ".fBasics.ReturnDistributions.12")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)    
    
    # Add Menu:
    Label = "Distribution Fits"
    subLabel = c(
        "* Example timeSeries: x = NYSE Returns",
        "Fit Normal Distribution", 
        "Fit Hyperbolic Distribution", 
        "Fit Normal Inverse Gaussian")
    Command = c(
        ".fBasics.DistributionFits.1",
        ".fBasics.DistributionFits.2",
        ".fBasics.DistributionFits.3",
        ".fBasics.DistributionFits.4")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)            

    # Add Menu:   
    Label = "One Sample Tests"
    subLabel = c(
        "* Example timeSeries: x = SP500 Returns",
        "Print fHTEST Class Representation",
        "Kolmogorov-Smirnov Normality Test",
        "Shapiro-Wilk Normality Test", 
        "Jarque-Bera Normality Test",
        "d'Agostino Normality Test",
        "Anderson-Darling Normality Test",
        "Cramer-von Mises Normality Test",
        "Lilliefors Normality Test",
        "Pearson Chi-Square Normality Test",
        "Shapiro-Francia Normality Test")
    Command = c(
        ".fBasics.OneSampleTests.1",
        ".fBasics.OneSampleTests.2",
        ".fBasics.OneSampleTests.3",
        ".fBasics.OneSampleTests.4",
        ".fBasics.OneSampleTests.5",
        ".fBasics.OneSampleTests.6",
        ".fBasics.OneSampleTests.7",
        ".fBasics.OneSampleTests.8",
        ".fBasics.OneSampleTests.9",
        ".fBasics.OneSampleTests.10",
        ".fBasics.OneSampleTests.11")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)

    # Add Menu:
    Label = "Two Sample Tests"
    subLabel = c(
        "* Example timeSeries: x = MSFT|SP500 Returns",
        "Print fHTEST Class Representation",
        "Kolmogorov-Smirnov Test",
        "Location: Unpaired t-Test",
        "Location: Kruskal-Wallis Test",
        "Variance: F Test",
        "Variance: Bartlett Test",
        "Variance: Fligner-Killeen Test",
        "Scale: Ansari-Bradley Test",
        "Scale: Mood Test",
        "Correlation: Pearson Test",
        "Correlation: Kendall's tau Test",
        "Correlation: Spearman's rho Test")
    Command = c(
        ".fBasics.TwoSampleTests.1",
        ".fBasics.TwoSampleTests.2",
        ".fBasics.TwoSampleTests.3",
        ".fBasics.TwoSampleTests.4",
        ".fBasics.TwoSampleTests.5",
        ".fBasics.TwoSampleTests.6",
        ".fBasics.TwoSampleTests.7",
        ".fBasics.TwoSampleTests.8",
        ".fBasics.TwoSampleTests.9",
        ".fBasics.TwoSampleTests.10",
        ".fBasics.TwoSampleTests.11",
        ".fBasics.TwoSampleTests.12",
        ".fBasics.TwoSampleTests.13")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Stylized Facts"
    subLabel = c(
        "* Example timeSeries: x = NYSE Returns",
        "Taylor Effect", 
        "Long Memory Behavior",
        "Log PDF Plot",
        "Normal QQ Plot",
        "Scaling Law Plot")
    Command = c(
        ".fBasics.StylizedFacts.1",
        ".fBasics.StylizedFacts.2",
        ".fBasics.StylizedFacts.3",
        ".fBasics.StylizedFacts.4",
        ".fBasics.StylizedFacts.5",
        ".fBasics.StylizedFacts.6")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)
  
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fBasicsMenu, Label = "fBasics")
            
}


################################################################################

