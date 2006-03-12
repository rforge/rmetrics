
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
# fMultivar Popup Menu

        
.fMultivar.PopupMenu =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Menu:
    fMultivarMenu <<- newToolbarMenu()
   
        # Add Menu:
    Label = "Technical Analysis"
    subLabel = c(
        "* Example Data: Open-High-Low-Close SP500",
        "emaTA - Exponential Moving Average",
        "biasTA - EMA Price Bias",
        "medpriceTA - Median Price",                    
        "typicalpriceTA - Typical Price",
        "wcloseTA - Weighted Close Price",
        "rocTA - Rate of Change",
        "oscTA - EMA-Oscillator",
        "momTA - Momentum Oscillator",
        "macdTA -  MACD Oscillator",
        "cdsTA - MACD Signal Line",
        "cdoTA - MACD Oscillator",
        "vohlTA - High/Low Volatility",
        "vorTA - Volatility Ratio",
        "fpkTA - Fast %K Stochastics",
        "fpdTA - Fast %D Stochastics",
        "spdTA - Slow %D Stochastics",
        "apdTA - Averaged %D Stochastics",
        "wprTA - Williams %R Stochastics",
        "rsiTA - Relative Strength Index",
        "... merge Series and Indicator")
    Command = c(
        ".fMultivar.TechnicalAnalysis.1",
        ".fMultivar.TechnicalAnalysis.2",
        ".fMultivar.TechnicalAnalysis.3",
        ".fMultivar.TechnicalAnalysis.4",
        ".fMultivar.TechnicalAnalysis.5",
        ".fMultivar.TechnicalAnalysis.6",
        ".fMultivar.TechnicalAnalysis.7",
        ".fMultivar.TechnicalAnalysis.8",
        ".fMultivar.TechnicalAnalysis.9",
        ".fMultivar.TechnicalAnalysis.10",
        ".fMultivar.TechnicalAnalysis.11",
        ".fMultivar.TechnicalAnalysis.12",
        ".fMultivar.TechnicalAnalysis.13",
        ".fMultivar.TechnicalAnalysis.14",
        ".fMultivar.TechnicalAnalysis.15",
        ".fMultivar.TechnicalAnalysis.16",
        ".fMultivar.TechnicalAnalysis.17",
        ".fMultivar.TechnicalAnalysis.18",
        ".fMultivar.TechnicalAnalysis.19",
        ".fMultivar.TechnicalAnalysis.20",
        ".fMultivar.TechnicalAnalysis.21")
    addToolbarMenu(fMultivarMenu, Label, subLabel, Command) 
        
    # Add Menu:
    Label = "Benchmark Analysis"
    subLabel = c(
        "* Example timeSeries: x = SP500 Index",
        "Compute Maximum Draw-Down",
        "Compute Sharpe Ratio",
        "Compute Sterling Ratio")
    Command = c(
        ".fMultivar.BenchmarkAnalysis.1",
        ".fMultivar.BenchmarkAnalysis.2",
        ".fMultivar.BenchmarkAnalysis.3",
        ".fMultivar.BenchmarkAnalysis.4")
    addToolbarMenu(fMultivarMenu, Label, subLabel, Command)     
        
    # Add Menu:
    Label = "Rolling Analysis"
    subLabel = c(
        "* Example timeSeries: x = SP500 Index",
        "Rolling Mean",
        "Rolling Variance",
        "Rolling Minimum",
        "Rolling Maximum")
    Command = c(
        ".fMultivar.RollingAnalysis.1",
        ".fMultivar.RollingAnalysis.2",
        ".fMultivar.RollingAnalysis.3",
        ".fMultivar.RollingAnalysis.4",
        ".fMultivar.RollingAnalysis.5")
    addToolbarMenu(fMultivarMenu, Label, subLabel, Command)
    
    # Add Menu:
    Label = "Regression Modelling"
    subLabel = c(
        "* Example Data: x = 0.7*x1 + 0.3*x2 + eps",
        "* Example Data: x = 10*sin(x1) + exp(x2) + eps",
        "* Example Data: x = Logit(0.7*x1 + 0.3*x2 + eps)",
        "LM- Linear Modelling",
        "GLM - Generalized Linear Modelling",
        "GAM - Generalized Additive Modelling",
        "PPR - Projection Pursuit Regression",
        "MARS - Multiadaptive Regression Splines",
        "POLYMARS - Polytochomous MARS Regression",
        "NNET - Neural Network Regression")
    Command = c(
        ".fMultivar.Regression.1",
        ".fMultivar.Regression.2",
        ".fMultivar.Regression.3",
        ".fMultivar.Regression.4",
        ".fMultivar.Regression.5",
        ".fMultivar.Regression.6",
        ".fMultivar.Regression.7",
        ".fMultivar.Regression.8",
        ".fMultivar.Regression.9",
        ".fMultivar.Regression.10")
    addToolbarMenu(fMultivarMenu, Label, subLabel, Command)
  
    # Add Menu:
    Label = "Regression Tests"
    subLabel = c(
        "* Example timeSeries: realInvest ~ realGNP + realInterest",
        "Breusch-Godfrey Test",
        "Breusch-Pagan Test",
        "Durbin-Watson Test",
        "Goldfeld-Quandt Test",
        "Harvey-Collier Test",
        "Harrison-McCabe Test",
        "Rainbow Test",
        "Ramsey RESET Test")
    Command = c(
        ".fMultivar.RegressionTests.1",
        ".fMultivar.RegressionTests.2",
        ".fMultivar.RegressionTests.3",
        ".fMultivar.RegressionTests.4",
        ".fMultivar.RegressionTests.5",
        ".fMultivar.RegressionTests.6",
        ".fMultivar.RegressionTests.7",
        ".fMultivar.RegressionTests.8",
        ".fMultivar.RegressionTests.9")
    addToolbarMenu(fMultivarMenu, Label, subLabel, Command)
        
    # Add Menu:
    # Label = "Equations Modelling"
    # subLabel = c(
    #     "Not yet implemented")
    # Command = c(
    #     ".fMultivar.EquationsModelling.1")
    # addToolbarMenu(fMultivarMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Matrix Addon"
    subLabel = c(
        "* Example: Pascal Matrix",
        "Return diag Matrix",
        "Return lower Triangular",
        "Return upper Triangular",
        "Compute the Determinant",
        "Compute the Inverse",
        "Compute the Norm",
        "Compute the Rank",
        "Compute the Transposed",
        "Exponentiate a Square Matrix",
        "Cholesky Factors",
        "Eigenvalues and Eigevectors",
        "Singular Value Decomposition",
        "Condition Number",
        "QR Decomposition")
    Command = c(
        ".fMultivar.MatrixAddon.1",
        ".fMultivar.MatrixAddon.2",
        ".fMultivar.MatrixAddon.3",
        ".fMultivar.MatrixAddon.4",
        ".fMultivar.MatrixAddon.5",
        ".fMultivar.MatrixAddon.6",
        ".fMultivar.MatrixAddon.7",
        ".fMultivar.MatrixAddon.8",
        ".fMultivar.MatrixAddon.9",
        ".fMultivar.MatrixAddon.10",
        ".fMultivar.MatrixAddon.11",
        ".fMultivar.MatrixAddon.12",
        ".fMultivar.MatrixAddon.13",
        ".fMultivar.MatrixAddon.14",
        ".fMultivar.MatrixAddon.15")
    addToolbarMenu(fMultivarMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Missing Values"
    subLabel = c(
        "* Example timeSeries: x = MSFT|SP500 Values",
        "Remove NAs", 
        "Interpolate NAs",
        "knn Algorithm",
        "* Example timeSeries: x = MSFT|SP500 Returns",
        "Substitute NAs") 
    Command = c(
        ".fMultivar.MissingValues.1",
        ".fMultivar.MissingValues.2",
        ".fMultivar.MissingValues.3",
        ".fMultivar.MissingValues.4",
        ".fMultivar.MissingValues.5",
        ".fMultivar.MissingValues.6")
    addToolbarMenu(fMultivarMenu, Label, subLabel, Command) 
        
    # Cascade fileMenu:
    cascadeToolbarMenu(Menu = fMultivarMenu, Label = "fMultivar")
        
}


################################################################################

