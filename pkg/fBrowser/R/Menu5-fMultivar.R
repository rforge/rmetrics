
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

        
.fMultivarPopup =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Menu:
    fMultivarMenu <<- tkNewPopup()
   
        # Add Menu:
    Label = "Technical Analysis"
    subLabel = c(
        "* Demo tS: Open-High-Low-Close SP500",
        "____________________________________",
        "Exponential Moving Average",
        "EMA Price Bias",
        "Median Price",                    
        "Typical Price",
        "Weighted Close Price",
        "Rate of Change",
        "EMA-Oscillator",
        "Momentum Oscillator",
        "MACD Oscillator",
        "MACD Signal Line",
        "cMACD Oscillator",
        "High/Low Volatility",
        "Volatility Ratio",
        "Fast %K Stochastics",
        "Fast %D Stochastics",
        "Slow %D Stochastics",
        "Averaged %D Stochastics",
        "Williams %R Stochastics",
        "Relative Strength Index")
    Command = c(
        ".fData.spcDaily",
        "tkSeparator",
        ".fMultivar.TechnicalAnalysis.emaTA",
        ".fMultivar.TechnicalAnalysis.biasTA",
        ".fMultivar.TechnicalAnalysis.medpriceTA",
        ".fMultivar.TechnicalAnalysis.typicalpriceTA",
        ".fMultivar.TechnicalAnalysis.wcloseTA",
        ".fMultivar.TechnicalAnalysis.rocTA",
        ".fMultivar.TechnicalAnalysis.oscTA",
        ".fMultivar.TechnicalAnalysis.momTA",
        ".fMultivar.TechnicalAnalysis.macdTA",
        ".fMultivar.TechnicalAnalysis.cdsTA",
        ".fMultivar.TechnicalAnalysis.cdoTA",
        ".fMultivar.TechnicalAnalysis.vohlTA",
        ".fMultivar.TechnicalAnalysis.vorTA",
        ".fMultivar.TechnicalAnalysis.fpkTA",
        ".fMultivar.TechnicalAnalysis.fpdTA",
        ".fMultivar.TechnicalAnalysis.spdTA",
        ".fMultivar.TechnicalAnalysis.apdTA",
        ".fMultivar.TechnicalAnalysis.wprTA",
        ".fMultivar.TechnicalAnalysis.rsiTA")
    tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command) 
        
    # Add Menu:
    Label = "Benchmark Analysis"
    subLabel = c(
        "* Demo tS: x = SP500 Index",
        "_____________________________",
        "Compute Maximum Draw-Down",
        "Compute Sharpe Ratio",
        "Compute Sterling Ratio")
    Command = c(
        ".fData.sp500IndexMonthly",
        "tkSeparator",
        ".fMultivar.BenchmarkAnalysis.2",
        ".fMultivar.BenchmarkAnalysis.3",
        ".fMultivar.BenchmarkAnalysis.4")
    tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command)     
        
    if (FALSE) {
    # Add Menu:
    Label = "Rolling Analysis"
    subLabel = c(
        "* Demo tS: x = SP500 Index",
        "__________________________",
        "Rolling Mean",
        "Rolling Variance",
        "Rolling Minimum",
        "Rolling Maximum")
    Command = c(
        ".fData.sp500IndexMonthly",
        "tkSeparator",
        ".fMultivar.RollingAnalysis.rollMean",
        ".fMultivar.RollingAnalysis.rollVar",
        ".fMultivar.RollingAnalysis.rollMin",
        ".fMultivar.RollingAnalysis.rollMax")
    tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command)
    }
    
    # Add Menu:
    Label = "________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command) 
       
    # Add Menu:
    Label = "Regression Modelling"
    subLabel = c(
        "* Demo Data: x = 0.7*x1 + 0.3*x2 + eps",
        "* Demo Data: x = 10*sin(x1) + exp(x2) + eps",
        "* Demo Data: x = Logit(0.7*x1 + 0.3*x2 + eps)",
        "_____________________________________________",
        "LM- Linear Modelling",
        "GLM - Generalized Linear Modelling",
        "GAM - Generalized Additive Modelling",
        "PPR - Projection Pursuit Regression",
        "MARS - Multiadaptive Regression Splines",
        "POLYMARS - Polytochomous MARS Regression",
        "NNET - Neural Network Regression")
    Command = c(
        ".fMultivar.Regression.lmData",
        ".fMultivar.Regression.glmData",
        ".fMultivar.Regression.gamData",
        "tkSeparator",
        ".fMultivar.Regression.lmFit",
        ".fMultivar.Regression.glmFit",
        ".fMultivar.Regression.gamFit",
        ".fMultivar.Regression.pprFit",
        ".fMultivar.Regression.marsFit",
        ".fMultivar.Regression.polymarsFit",
        ".fMultivar.Regression.nnetFit")
    tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command)
  
    # Add Menu:
    Label = "Regression Tests"
    subLabel = c(
        "* Demo Data: x = 0.7*x1 + 0.3*x2 + eps",
        "____________________________________________",
        "Breusch-Godfrey Test",
        "Breusch-Pagan Test",
        "Durbin-Watson Test",
        "Goldfeld-Quandt Test",
        "Harvey-Collier Test",
        "Harrison-McCabe Test",
        "Rainbow Test",
        "Ramsey RESET Test")
    Command = c(
        ".fMultivar.Regression.lmData",
        "tkSeparator",
        ".fMultivar.RegressionTests.bgTest",
        ".fMultivar.RegressionTests.bpTest",
        ".fMultivar.RegressionTests.dwTest",
        ".fMultivar.RegressionTests.gqTest",
        ".fMultivar.RegressionTests.harvestTest",
        ".fMultivar.RegressionTests.hmcTest",
        ".fMultivar.RegressionTests.rainTest",
        ".fMultivar.RegressionTests.resetTest")
    tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command) 
    
    # Add Menu:
    # Label = "Equations Modelling"
    # subLabel = c(
    #     "Not yet implemented")
    # Command = c(
    #     ".fMultivar.EquationsModelling.1")
    # tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Matrix Addon"
    subLabel = c(
        "* Demo Matrix: Pascal Matrix",
        "____________________________",
        "Return diag Matrix",
        "Return lower Triangular",
        "Return upper Triangular",
        "Compute the Determinant",
        "Compute the Inverse",
        "Compute the Norm",
        "Compute the Rank",
        "Compute the Transposed",
        "Exponentiate a Square Matrix",
        "____________________________",
        "Cholesky Factors",
        "Eigenvalues and Eigevectors",
        "Singular Value Decomposition",
        "Condition Number",
        "QR Decomposition")
    Command = c(
        ".fMultivar.MatrixAddon.1",
        "tkSeparator",
        ".fMultivar.MatrixAddon.2",
        ".fMultivar.MatrixAddon.3",
        ".fMultivar.MatrixAddon.4",
        ".fMultivar.MatrixAddon.5",
        ".fMultivar.MatrixAddon.6",
        ".fMultivar.MatrixAddon.7",
        ".fMultivar.MatrixAddon.8",
        ".fMultivar.MatrixAddon.9",
        ".fMultivar.MatrixAddon.10",
        "tkSeparator",
        ".fMultivar.MatrixAddon.11",
        ".fMultivar.MatrixAddon.12",
        ".fMultivar.MatrixAddon.13",
        ".fMultivar.MatrixAddon.14",
        ".fMultivar.MatrixAddon.15")
    tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Missing Values"
    subLabel = c(
        "* Demo tS: x = MSFT|SP500 Values",
        "___________________________________",
        "Remove NAs", 
        "Interpolate NAs",
        "knn Algorithm")
    Command = c(
        ".fMultivar.MissingValues.1",
        "tkSeparator",
        ".fMultivar.MissingValues.2",
        ".fMultivar.MissingValues.3",
        ".fMultivar.MissingValues.4")
    tkAddPopupMenu(fMultivarMenu, Label, subLabel, Command) 
        
    # Cascade fileMenu:
    tkCascadePopup(Menu = fMultivarMenu, Label = "fMultivar")        
}


################################################################################

