
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
#  fOptions Popup Menu


.fOptionsPopup =
function()
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   fOptions Popup Menu
    
    # FUNCTION:
    
    # Menu:
    fOptionsMenu <<- tkNewPopup()
   
    # Add Menu:
    Label = "Plain Vanilla Options"
    subLabel = c(
        "Black Scholes", 
        "Black 76", 
        "Miltersen Schwartz" )
    Command = c(
        ".fOptions.PlainVanilla.1",
        ".fOptions.PlainVanilla.2",
        ".fOptions.PlainVanilla.3")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
    
    if (FALSE) { ###
    # Add Menu:
    Label = "Basic American Options"
    subLabel = c(
        "Roll Geske Whaley Option",
        "BAW American Approximated Option",
        "BS American Approximated Option" )
    Command = c(
        ".fOptions.BasicAmerican.1",
        ".fOptions.BasicAmerican.2",
        ".fOptions.BasicAmerican.3",)
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)    
    } ###
    
    # Add Menu:
    Label = "Binomial Tree Options"
    subLabel = c(
        "CRR Binomial Tree Option",
        "JR Binomial Tree Option",
        "TIAN BinomialTree Option")
    Command = c(
        ".fOptions.BinomialTree.1",
        ".fOptions.BinomialTree.2",
        ".fOptions.BinomialTree.3")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command) 
        
    # Add Menu:
    Label = "____________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command) 
    
    # Add Menu:
    Label = "Multiple Exercises Options"
    subLabel = c(
        "Executive Stock",
        "Forward Start",
        "Ratchet",
        "Time Switch",
        "Simple Chooser",
        "Complex Chooser",
        "Option On Option",
        "Holder Extendible",
        "Writer Extendible")
    Command = c(
        ".fOptions.MultipleExercises.1",
        ".fOptions.MultipleExercises.2",
        ".fOptions.MultipleExercises.3",
        ".fOptions.MultipleExercises.4",
        ".fOptions.MultipleExercises.5",
        ".fOptions.MultipleExercises.6",
        ".fOptions.MultipleExercises.7",
        ".fOptions.MultipleExercises.8",
        ".fOptions.MultipleExercises.9")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
           
    # Add Menu:
    Label = "Multiple Assets Options"
    subLabel = c(
        "Two Asset Correlation",
        "European On Exchange",
        "American Exchange",
        "Exchange Exchange",
        "Option On The MinMax",
        "Spread Approximated")   
    Command = c(
        ".fOptions.MultipleAssets.1",
        ".fOptions.MultipleAssets.2",
        ".fOptions.MultipleAssets.3",
        ".fOptions.MultipleAssets.4",
        ".fOptions.MultipleAssets.5",
        ".fOptions.MultipleAssets.6")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Lookback Options"
    subLabel = c(
        "Floating Strike Lookback",
        "Fixed Strike Lookback",
        "Partial Floating Strike LB",
        "Partial Fixed Strike LB", 
        "Extreme Spread Option")
    Command = c(
        ".fOptions.Lookback.1",
        ".fOptions.Lookback.2",
        ".fOptions.Lookback.3",
        ".fOptions.Lookback.4",
        ".fOptions.Lookback.5")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
           
    # Add Menu:
    Label = "Barrier Options"
    subLabel = c(
        "Standard Barrier",
        "Double Barrier",
        "Partial Time Barrier",
        "Two Asset Barrier",
        "Partial Time TwoAsset Barrier",
        "Look Barrier",
        "Discrete Adjusted Barrier",
        "Soft Barrier")
    Command = c(
        ".fOptions.Barrier.1",
        ".fOptions.Barrier.2",
        ".fOptions.Barrier.3",
        ".fOptions.Barrier.4",
        ".fOptions.Barrier.5",
        ".fOptions.Barrier.6",
        ".fOptions.Barrier.7",
        ".fOptions.Barrier.8")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
       
    # Add Menu:
    Label = "Binary Options"
    subLabel = c(
        "Gap Option",
        "Cash Or Nothing",
        "Two Asset Cash Or Nothing",
        "Asset Or Nothing",
        "Super Share",
        "Binary Barrier")
    Command = c(
        ".fOptions.Binary.1",
        ".fOptions.Binary.2",
        ".fOptions.Binary.3",
        ".fOptions.Binary.4",
        ".fOptions.Binary.5",
        ".fOptions.Binary.6")
   tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Asian Options"
    subLabel = c(
        "Geometric Average Rate",
        "Turnbull-Wakeman Approximated Asian",
        "Levy Approximated Asian")
    Command = c(
        ".fOptions.Asian.1",
        ".fOptions.Asian.2",
        ".fOptions.Asian.3")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
        
    # Add Menu:
    Label = "Currency Translated Options"
    subLabel = c(
        "FX In Domestic Currency",
        "Quanto",
        "Equity Linked FX",
        "Takeover FX Option")
    Command = c(
        ".fOptions.CurrencyTranslated.1",
        ".fOptions.CurrencyTranslated.2",
        ".fOptions.CurrencyTranslated.3",
        ".fOptions.CurrencyTranslated.4")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
                   
    # Add Menu:
    Label = "____________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command) 
    
    # Add Menu:
    if (FALSE) {
    Label = "Heston Nandi Garch Fit"
    subLabel = c(
       "Simulate HN-GARCH Process",
       "Fit HN-GARCH",
       "... Print Summary Report",
       "... Print Statistics")
    Command = c(
       ".fOptions.HestonNandi.1",
       ".fOptions.HestonNandi.2",
       ".fOptions.HestonNandi.3",
       ".fOptions.HestonNandi.4")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command) } 
    
    # Add Menu:
    if (FALSE) {
    Label = "Heston Nandi Options"
    subLabel = c(
       "Not yet implemented")
    Command = c(
        ".fOptions.C2Cmd.1")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command) }
        
    # Add Menu:
    Label = "Low Discrepancy Sequences"
    subLabel = c(
        "Uniform Pseudo",
        "Normal Pseudo",
        "Uniform Halton",
        "Normal Halton",
        "Uniform Sobol",
        "Normal Sobol")
    Command = c(
        ".fOptions.LowDiscrepancy.1",
        ".fOptions.LowDiscrepancy.2",
        ".fOptions.LowDiscrepancy.3",
        ".fOptions.LowDiscrepancy.4",
        ".fOptions.LowDiscrepancy.5",
        ".fOptions.LowDiscrepancy.6")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)    
        
    # Add Menu:
    Label = "Monte Carlo Options"
    subLabel = c(
        "Plain Vanilla Payoff with Normal Innovations", 
        "Plain Vanilla Payoff with Sobol Innovations", 
        "Arithmetic Asian Payoff with Normal Innovations",
        "Arithmetic Asian Payoff with Sobol Innovations") 
    Command = c(
        ".fOptions.MonteCarlo.1",
        ".fOptions.MonteCarlo.2",
        ".fOptions.MonteCarlo.3",
        ".fOptions.MonteCarlo.4")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
        
    # Add Menu:
    if (FALSE) {
    Label = "Exponential BM"
    subLabel = c(
       "Not yet implemented")
    Command = c(
       ".fOptions.ExponentialBM.1")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command) }
           
    if (FALSE) {
    # Add Menu:
    Label = "____________________________"
    subLabel = 
        NULL
    Command = 
        "tkSeparator"
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command) 
    
    # Add Menu:
    Label = "Special Functions"
    subLabel = c(
        "Gamma Function Plot",
        "Psi Function Plot",
        "Complex Gamma Function Plot",
        "______________________________",
        "Kummer Function Slider",
        "Whittaker Function Slider",
        "______________________________",
        "Bessel Function Slider")
    Command = c(
        ".fOptions.Gamma.1",
        ".fOptions.Gamma.2",
        ".fOptions.Gamma.3",
        "tkSeparator",
        ".fOptions.Hypergeometric.1",
        ".fOptions.Hypergeometric.2",
        "tkSeparator",
        ".fOptions.Bessel.1")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command)
        
    # Add Menu:
    if (FALSE) {
    Label = "EBM Asian Options"
    subLabel = c(
        "Not yet implemented")
    Command = c(
        ".fOptions.EBMAsian.1")
    tkAddPopupMenu(fOptionsMenu, Label, subLabel, Command) }
    }
 
    # Cascade fileMenu:
    tkCascadePopup(Menu = fOptionsMenu, Label = "fOptions")                 
}       
        

################################################################################

