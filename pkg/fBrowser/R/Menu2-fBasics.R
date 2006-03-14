
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
# fBasics Popup Menu


.fBasics.PopupMenu =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fBasics Popup Menu
    
    # FUNCTION:
    
    # Menu:
    fBasicsMenu <<- newToolbarMenu()
     
    # Add Menu:
    Label = "Time Series Import"
    subLabel = c(
        "Economagic Series Download", 
        "Yahoo Series Download",
        "FRED St. Louis Series Download")
    Command = c(
        ".fBasics.Import.economagicImport",
        ".fBasics.Import.yahooImport",
        ".fBasics.Import.fredImport")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)
 
    
    # Add Menu:
    Label = "Time Series Plots"
    subLabel = c(
        "* Example timeSeries: x = SP500 Returns",
        "Univariate Time Series Plot",
        "Autocorrelation Function Plot",
        "Partial ACF Plot",
        "Histogram Plot",
        "* Example timeSeries: x = MSFT|SP500 Returns",
        "Bivariate Time Series Plot",
        "Scatterdiagramm Plot")
    Command = c(
        ".fData.sp500Monthly",
        ".fBasics.PlotFunctions.plot",
        ".fBasics.PlotFunctions.acfPlot",
        ".fBasics.PlotFunctions.pacfPlot",
        ".fBasics.PlotFunctions.histPlot",
        ".fData.msftsp500Monthly",
        ".fBasics.PlotFunctions.bivariatePlot",
        ".fBasics.PlotFunctions.scatterPlot")
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
        ".fData.nyseDaily",
        ".fBasics.StylizedFacts.teffectPlot",
        ".fBasics.StylizedFacts.lmacfPlot",
        ".fBasics.StylizedFacts.logpdfPlot",
        ".fBasics.StylizedFacts.qqgaussPlot",
        ".fBasics.StylizedFacts.scalinglawPlot")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)
  
        
     # Add Menu:
    Label = "Time Series Statistics"
    subLabel = c(
        "* Example timeSeries: x = SP500 Returns",
        "mean - Compute Mean",
        "var - Compute Variance",
        "skewness - Compute Skewness",
        "kurtosis - Compute Kurtosis",
        "summary - Summary Report",
        "basicStats - Basic Statistics")
    Command = c(
        ".fData.sp500Monthly",
        ".fBasics.BasicStatistics.mean",
        ".fBasics.BasicStatistics.var",
        ".fBasics.BasicStatistics.skewness",
        ".fBasics.BasicStatistics.kurtosis",
        ".fBasics.BasicStatistics.summary",
        ".fBasics.BasicStatistics.basicStats")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Return Distributions"
    subLabel = c(
        "* Example timeSeries: x - Daily NYSE Returns",
        "Generate Normal Random Numbers",
        "... Random Variates Slider",
        "... Distribution Slider",
        "... Fit Normal Distribution",
        "Generate Hyperbolic Random Numbers",
        "... Random Variates Slider",
        "... Distribution Slider",
        "... Fit Hyperbolic Distribution",
        "Generate Normal Inveres Gaussian Numbers",
        "... Random Variates Slider",
        "... Distribution Slider",
        "... Fit Normal Inverse Gaussian",
        "Generate Symmetric Stable Random Numbers",
        "... Random Variates Slider",
        "... Distribution Slider",
        "Generate Stable Random Numbers",
        "... Random Variates Slider",
        "... Distribution Slider")
    Command = c(
        ".fData.nyseDaily",
        ".fBasics.ReturnDistributions.rnorm",
        ".fBasics.ReturnDistributions.rnormSlider",
        ".fBasics.ReturnDistributions.dnormSlider",
        ".fBasics.DistributionFits.normFit",
        ".fBasics.ReturnDistributions.rhyp",
        ".fBasics.ReturnDistributions.rhypSlider",
        ".fBasics.ReturnDistributions.dhypSlider",
        ".fBasics.DistributionFits.hypFit",
        ".fBasics.ReturnDistributions.rnig",
        ".fBasics.ReturnDistributions.rnigSlider",
        ".fBasics.ReturnDistributions.dnigSlider",
        ".fBasics.DistributionFits.nigFit",
        ".fBasics.ReturnDistributions.rsymstb",
        ".fBasics.ReturnDistributions.rsymstbSlider",
        ".fBasics.ReturnDistributions.dsymstbSlider",
        ".fBasics.ReturnDistributions.rstable",
        ".fBasics.ReturnDistributions.rstableSlider",
        ".fBasics.ReturnDistributions.dstableSlider")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)             

    # Add Menu:   
    Label = "Normality Tests"
    subLabel = c(
        "* Example timeSeries: x = SP500 Returns",
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
        ".fData.sp500Monthly",
        ".fBasics.NormalityTests.ksnormTest",
        ".fBasics.NormalityTests.shapiroTest",
        ".fBasics.NormalityTests.jarqueberaTest",
        ".fBasics.NormalityTests.dagoTest",
        ".fBasics.NormalityTests.adTest",
        ".fBasics.NormalityTests.cvmTest",
        ".fBasics.NormalityTests.lillieTest",
        ".fBasics.NormalityTests.pchiTest",
        ".fBasics.NormalityTests.sfTest")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)

    # Add Menu:
    Label = "Bivariate Sample Tests"
    subLabel = c(
        "* Example timeSeries: x = MSFT|SP500 Returns",
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
        ".fData.msftsp500Monthly",
        ".fBasics.BivariateTests.ks2Test",
        ".fBasics.BivariateTests.tTest",
        ".fBasics.BivariateTests.kw2Test",
        ".fBasics.BivariateTests.varfTest",
        ".fBasics.BivariateTests.bartlett2Test",
        ".fBasics.BivariateTests.fligner2Test",
        ".fBasics.BivariateTests.ansariTest",
        ".fBasics.BivariateTests.moodTest",
        ".fBasics.BivariateTests.pearsonTest",
        ".fBasics.BivariateTests.kendallTest",
        ".fBasics.BivariateTests.spearmanTest")
    addToolbarMenu(fBasicsMenu, Label, subLabel, Command)
    
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fBasicsMenu, Label = "fBasics")
            
}


################################################################################

