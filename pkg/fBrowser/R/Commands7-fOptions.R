
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA. 

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
# PlainVanillaOptions


.fOptions.PlainVanilla.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # GBS Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma, object2x, 
        report) {
        object <<- GBSOption(TypeFlag = TypeFlag, S = S, X = X, 
            Time = Time, r = r, b = b, sigma = sigma) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 100, 
            Time = 1, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "GBS Option" ) 
}


# ------------------------------------------------------------------------------


.fOptions.PlainVanilla.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Black76 Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, FT, X, Time, r, sigma, object2x, 
        report) {
        object <<- Black76Option(TypeFlag = TypeFlag, FT = FT, X = X, 
            Time = Time, r = r, sigma = sigma) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            FT = 95, 
            X = 80, 
            Time = 0.5, 
            r = 0.05, 
            sigma = 0.266,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Black76 Option" ) 
}
    

# ------------------------------------------------------------------------------

    
.fOptions.PlainVanilla.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # Miltersen Schwartz Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, Pt, FT, X, time, Time, sigmaS, sigmaE,
        sigmaF, rhoSE, rhoSF, rhoEF, KappaE, KappaF, object2x, report) {
        object <<- MiltersenSchwartzOption(TypeFlag = TypeFlag, Pt = Pt, 
            FT = FT, X = X, time = time, Time = Time, sigmaS = sigmaS, 
            sigmaE = sigmaE, sigmaF = sigmaF, rhoSE = rhoSE, 
            rhoSF = rhoSF, rhoEF = rhoEF, KappaE = KappaE, 
            KappaF = KappaF)  
        object }
    tkExecute(
        fun = myFunction, 
        params = list(
            TypeFlag = "c", 
            Pt = 0.9876, 
            FT = 95,  
            X = 80, 
            time = 0.25, 
            Time = 0.50, 
            sigmaS = 0.2660, 
            sigmaE = 0.2490, 
            sigmaF = 0.0096, 
            rhoSE = 0.8050, 
            rhoSF = 0.0805, 
            rhoEF = 0.1243, 
            KappaE = 1.0450, 
            KappaF = 0.2000,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Miltersen Schwartz Option" ) 
}


################################################################################
# Basic American Options


.fOptions.BasicAmerican.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Roll Geske Whaley Option:
    helpTopic <<- ""
    myFunction = function(S, X, time1, Time2, r, D, sigma, object2x, 
        report) {
        object <<- RollGeskeWhaleyOption(S = S, X = X, time1 = time1, 
            Time2 = Time2, r = r, D = D, sigma = sigma) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            S = 80, 
            X = 82, 
            time1 = 0.25, 
            Time2 = 0.3333, 
            r = 0.06, 
            D = 4, 
            sigma = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Roll Geske Whaley Option" )
}


# ------------------------------------------------------------------------------


.fOptions.BasicAmerican.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # BAW American Approx Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma, 
        object2x, report) {
        object <<- BAWAmericanApproxOption(TypeFlag = TypeFlag, 
            S = S, X = X, Time = Time, r = r, b = b, sigma = sigma) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "p", 
            S = 100, 
            X = 100, 
            Time = 0.5, 
            r = 0.10, 
            b = 0, 
            sigma = 0.25,
            object2x = FALSE,
            report = TRUE ),
        infoName = "BAW American Option" )
}


# ------------------------------------------------------------------------------


.fOptions.BasicAmerican.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # BS American Approx Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma, 
        object2x, report) {
        object <<- BSAmericanApproxOption(TypeFlag = TypeFlag, 
            S = S, X = X, Time = Time, r = r, b = b, sigma = sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 42, 
            X = 40, 
            Time = 0.75, 
            r = 0.04, 
            b = - 0.04, 
            sigma = 0.35,
            object2x = FALSE,
            report = TRUE ),
        infoName = "BS American Option" )
}


################################################################################
# Binomial Tree Options


.fOptions.BinomialTree.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # CRR Binomial Tree Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time,  r, b, sigma, n, 
        object2x, report) {
        object <<- CRRBinomialTreeOption(TypeFlag = TypeFlag, S = S, 
            X = X, Time = Time, r = r, b = b, sigma = sigma, n = n)
            }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "pa", 
            S = 50, X = 50, 
            Time = 0.4167, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.40, 
            n = 50,
            object2x = FALSE,
            report = TRUE ),
        infoName = "CRRBinomialTreeOption" )
}


# ------------------------------------------------------------------------------


.fOptions.BinomialTree.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # JR Binomial Tree Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time,  r, b, sigma, n, 
        object2x, report) {
        object <<- JRBinomialTreeOption(TypeFlag = TypeFlag, S = S, 
            X = X, Time = Time, r = r, b = b, sigma = sigma, n = n)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "pa", 
            S = 50, 
            X = 50, 
            Time = 0.4167, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.40, 
            n = 50,
            object2x = FALSE,
            report = TRUE ),
        infoName = "JRBinomialTreeOption" )
}


# ------------------------------------------------------------------------------


.fOptions.BinomialTree.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # TIAN Binomial Tree Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma, n, 
        object2x, report) {
        object <<- TIANBinomialTreeOption(TypeFlag = TypeFlag, S = S, 
            X = X, Time = Time, r = r, b = b, sigma = sigma, n = n)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "pa", 
            S = 50, 
            X = 50, 
            Time = 0.4167, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.40, 
            n = 50,
            object2x = FALSE,
            report = TRUE ),
        infoName = "TIANBinomialTreeOption" )
}


# ------------------------------------------------------------------------------


.fOptions.BinomialTree.4 =
function()
{   # A function implemented by Diethelm Wuertz

    # Binomial Tree Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time,  r, b, sigma, n, 
        object2x, report) {
        object <<- BinomialTreeOption(TypeFlag = TypeFlag, S = S, 
            X = X, Time = Time, r = r, b = b, sigma = sigma, n = n)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "pa", 
            S = 50, 
            X = 50, 
            Time = 0.4167, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.40, 
            n = 50,
            object2x = FALSE,
            report = TRUE ),
        infoName = "BinomialTreeOption" )
}

    
################################################################################
# Multiple Exercises Options


.fOptions.MultipleExercises.1 =
function()
{   # A function implemented by Diethelm Wuertz
 
    # Executive Stock Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma, lambda, 
        object2x, report) {
        object <<- ExecutiveStockOption(TypeFlag, S, X, Time, 
            r, b, sigma, lambda)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 60, 
            X = 64, 
            Time = 2, 
            r = 0.07, 
            b = 0.04, 
            sigma = 0.38, 
            lambda = 0.15,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Executive Stock Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleExercises.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Forward Start Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, alpha, time1, Time2, r, b, 
        sigma, object2x, report) {
        object <<- ForwardStartOption(TypeFlag, S, alpha, time1, 
            Time2, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 60, 
            alpha = 1.1, 
            time1 = 1.00, 
            Time2 = 0.25, 
            r = 0.08, 
            b = 0.04, 
            sigma = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Forward Start Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleExercises.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # Ratchet Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, alpha, time1, Time2, r, b, 
        sigma, object2x, report) {
        object <<- RatchetOption(TypeFlag, S, alpha, time1, Time2, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 60, 
            alpha = 1.1, 
            time1 = c(1.00, 0.75), 
            Time2 = c(0.75, 0.50), 
            r = 0.08, 
            b = 0.04, 
            sigma = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Ratchet Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleExercises.4 =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Time Switch Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma, A, m, dt, 
        object2x, report) {
        object <<- TimeSwitchOption(TypeFlag, S, X, Time, r, b, sigma, 
            A, m, dt)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 110, 
            Time = 1, 
            r = 0.06, 
            b = 0.06, 
            sigma = 0.26, 
            A = 5, 
            m = 0, 
            dt = 0.002739726,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Time Switch Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleExercises.5 =
function()
{   # A function implemented by Diethelm Wuertz

    # Simple Chooser Option:
    helpTopic <<- ""
    myFunction = function(S, X, time1, Time2, r, b, sigma, 
        object2x, report) {
        object <<- SimpleChooserOption(S, X, time1, Time2, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            S = 50, 
            X = 50, 
            time1 = 0.25, 
            Time2 = 0.50, 
            r = 0.08, 
            b = 0.08, 
            sigma = 0.25,
            object2x = FALSE,
            report = TRUE ),
        infoName = "SimpleChooserOption" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleExercises.6 =
function()
{   # A function implemented by Diethelm Wuertz

    # Complex Chooser Option:
    helpTopic <<- ""
    myFunction = function(S, Xc, Xp, Time, Timec, Timep, r, b, sigma, 
        object2x, report) {
        object <<- ComplexChooserOption(S, Xc, Xp, Time, Timec, Timep, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            S = 50, 
            Xc = 55, 
            Xp = 48, 
            Time = 0.25, 
            Timec = 0.50, 
            Timep = 0.5833, 
            r = 0.10, 
            b = 0.05, 
            sigma = 0.35, 
            doprint = FALSE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Complex Chooser Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleExercises.7 =
function()
{   # A function implemented by Diethelm Wuertz

    # Option On Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X1, X2, time1, Time2, r, b, 
        sigma, object2x, report) {
        object <<- OptionOnOption(TypeFlag, S, X1, X2, time1, Time2, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "pc", 
            S = 500, 
            X1 = 520, 
            X2 = 50, 
            time1 = 0.50, 
            Time2 = 0.25, 
            r = 0.08, 
            b = 0.05, 
            sigma = 0.35,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Option On Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleExercises.8 =
function()
{   # A function implemented by Diethelm Wuertz

    # Holder Extendible Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X1, X2, time1, Time2, r, b, 
        sigma, A, object2x, report) {
        object <<- HolderExtendibleOption(TypeFlag, S, X1, X2, time1, 
            Time2, r, b, sigma, A)
        object }
    tkExecute(
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X1 = 100, 
            X2 = 105, 
            time1 = 0.50, 
            Time2 = 0.75, 
            r = 0.08, 
            b = 0.08, 
            sigma = 0.25, 
            A = 1,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Holder Extendible Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleExercises.9 =
function()
{   # A function implemented by Diethelm Wuertz

    # Writer Extendible Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X1, X2, time1, Time2, r, b, 
        sigma, object2x, report) {
        object <<- WriterExtendibleOption(TypeFlag, S, X1, X2, time1, 
            Time2, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 80, 
            X1 = 90, 
            X2 = 82,
            time1 = 0.50, 
            Time2 = 0.75, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Writer Extendible Option" )
}


################################################################################
# Multiple Assets Options


.fOptions.MultipleAssets.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Two Asset Correlation Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S1, S2, X1, X2, Time, r, b1, b2,
        sigma1, sigma2, rho, object2x, report) {
        object <<- TwoAssetCorrelationOption(TypeFlag, S1, S2, X1, X2, 
            Time, r, b1, b2, sigma1, sigma2, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S1 = 52, 
            S2 = 65, 
            X1 = 50, 
            X2 = 70, 
            Time = 0.5, 
            r = 0.10, 
            b1 = 0.10, 
            b2 = 0.10, 
            sigma1 = 0.2, 
            sigma2 = 0.3, 
            rho = 0.75,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Two Asset Correlation Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleAssets.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Two Asset Correlation Option:
    helpTopic <<- ""
    myFunction = function(S1, S2, Q1, Q2, Time, r, b1, b2, sigma1, 
        sigma2, rho, object2x, report) {
        object <<- TwoAssetCorrelationOption(S1, S2, Q1, Q2, Time,  
            r, b1, b2, sigma1, sigma2, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            S1 = 22, 
            S2 = 0.20, 
            Q1 = 1, 
            Q2 = 1, 
            Time = 0.1, 
            r = 0.1, 
            b1 = 0.04, 
            b2 = 0.06, 
            sigma1 = 0.2, 
            sigma2 = 0.25, 
            rho = -0.5,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Two Asset Correlation Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleAssets.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # American Exchange Option:
    helpTopic <<- ""
    myFunction = function(S1, S2, Q1, Q2, Time, r, b1, b2, sigma1, 
        sigma2, rho, object2x, report) {
        object <<- AmericanExchangeOption(S1, S2, Q1, Q2, Time, r, 
            b1, b2, sigma1, sigma2, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            S1 = 22, 
            S2 = 0.20, 
            Q1 = 1, 
            Q2 = 1, 
            Time = 0.1, 
            r = 0.1, 
            b1 = 0.04, 
            b2 = 0.06, 
            sigma1 = 0.2, 
            sigma2 = 0.25, 
            rho = -0.5,
            object2x = FALSE,
            report = TRUE ),
        infoName = "American Exchange Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleAssets.4 =
function()
{   # A function implemented by Diethelm Wuertz

    # Exchange On Exchange Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S1, S2, Q, time1, Time2, r, b1, b2, 
        sigma1, sigma2, rho, object2x, report) {
        object <<- ExchangeOnExchangeOption(TypeFlag, S1, S2, Q, time1,  
            Time2, r, b1, b2, sigma1, sigma2, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = as.character(flag), 
            S1 = 105, 
            S2 = 100, 
            Q = 0.1, 
            time1 = 0.75, 
            Time2 = 1.00, 
            r = 0.10, 
            b1 = 0.10, 
            b2 = 0.10, 
            sigma1 = 0.20, 
            sigma2 = 0.25, 
            rho = -0.5,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Exchange On Exchange Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleAssets.5 =
function()
{   # A function implemented by Diethelm Wuertz
    # Two Risky Assets Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S1, S2, X, Time, r, b1, b2, sigma1, 
        sigma2, rho, object2x, report) {
        object <<- TwoRiskyAssetsOption(TypeFlag, S1, S2, X, Time, r, 
            b1, b2, sigma1, sigma2, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "cmax", 
            S1 = 100, 
            S2 = 105, 
            X = 98, 
            Time = 0.5, 
            r = 0.05, 
            b1 = -0.01, 
            b2 = -0.04, 
            sigma1 = 0.11, 
            sigma2 = 0.16, 
            rho = 0.63,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Two Risky Assets Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MultipleAssets.6 =
function()
{   # A function implemented by Diethelm Wuertz

    # Spread Approx Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S1, S2, X, Time, r, sigma1, sigma2, 
        rho, object2x, report) {
        object <<- SpreadApproxOption(TypeFlag, S1, S2, X, Time, r, 
            sigma1, sigma2, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S1 = 28, 
            S2 = 20, 
            X = 7, 
            Time = 0.25, 
            r = 0.05, 
            sigma1 = 0.29, 
            sigma2 = 0.36, 
            rho = 0.42,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Spread Approx Option" )
}


################################################################################
# Lookback Options


.fOptions.Lookback.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Floating Strike Lookback Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, SMinOrMax, Time, r, b, sigma, 
        object2x, report) {
        object <<- FloatingStrikeLookbackOption(TypeFlag, S, SMinOrMax, 
            Time, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 120, 
            SMinOrMax = 100, 
            Time = 0.5, 
            r = 0.10, 
            b = 0.04, 
            sigma = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Floating Strike Lookback Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Lookback.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Fixed Strike Lookback Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, SMinOrMax, X, Time, r, b, 
        sigma, object2x, report) {
        object <<- FixedStrikeLookbackOption(TypeFlag, S, SMinOrMax, 
            X, Time, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            SMinOrMax = 100, 
            X = 105, 
            Time = 0.5, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Fixed Strike Lookback Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Lookback.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # PT Floating Strike Lookback Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, SMinOrMax, time1, Time2, r, 
        b, sigma, lambda, object2x, report) {
        object <<- PTFloatingStrikeLookbackOption(TypeFlag, S, 
            SMinOrMax, time1, Time2, r, b, sigma, lambda)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "p", 
            S = 90, 
            SMinOrMax = 90, 
            time1 = 0.5, 
            Time2 = 1.0, 
            r = 0.06, 
            b = 0.06, 
            sigma = 0.20, 
            lambda = 1,
            object2x = FALSE,
            report = TRUE ),
        infoName = "PT Floating Strike Lookback Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Lookback.4 =
function()
{   # A function implemented by Diethelm Wuertz

    # PT Fixed Strike Lookback Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, time1, Time2, r, b, sigma, 
        object2x, report) {
        object <<- PTFixedStrikeLookbackOption(TypeFlag, S, X, time1, 
            Time2, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 90, 
            time1 = 0.5,  
            Time2 = 1.0, 
            r = 0.06, 
            b = 0.06, 
            sigma = 0.20,
            object2x = FALSE,
            report = TRUE ),
        infoName = "PT Fixed Strike Lookback Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Lookback.5 =
function()
{   # A function implemented by Diethelm Wuertz

    # Extreme Spread Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, SMin, SMax, time1, Time2, 
        r, b, sigma, object2x, report) {
        object <<- ExtremeSpreadOption(TypeFlag, S, SMin, SMax, time1, 
            Time2, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            SMin = NA, 
            SMax = 110, 
            time1 = 0.5, 
            Time2 = 1, 
            r = 0.1, 
            b = 0.1, 
            sigma = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Extreme Spread Option" )    
}


################################################################################
# Barrier Options


.fOptions.Barrier.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Standard Barrier Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, H, K, Time, r, b, sigma, 
        object2x, report) {
        object <<- StandardBarrierOption(TypeFlag, S, X, H, K, 
            Time, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "cdo", 
            S = 100, 
            X = 90, 
            H = 95, 
            K = 3, 
            Time = 0.5, 
            r = 0.08, 
            b = 0.04, 
            sigma = 0.25,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Standard Barrier Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Barrier.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Double Barrier Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, L, U, Time, r, b, sigma, 
        delta1, delta2, object2x, report) {
        object <<- DoubleBarrierOption(TypeFlag, S, X, L, U, Time,  
            r, b, sigma, delta1, delta2)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "co", 
            S = 100, 
            X = 100, 
            L = 50, 
            U = 150, 
            Time = 0.25, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.15, 
            delta1 = -0.1, 
            delta2 = 0.1,
            object2x = FALSE,
            report = TRUE ),
        infoName = "DoubleB arrier Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Barrier.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # PT Single Asset Barrier Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, H, time1, Time2, r, b, 
        sigma, object2x, report) {
        object <<- PTSingleAssetBarrierOption(TypeFlag, S, X, H,  
            time1, Time2, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "coB1", 
            S = 95, 
            X = 110, 
            H = 100, 
            time1 = 0.5, 
            Time2 = 1, 
            r = 0.20, 
            b = 0.20, 
            sigma = 0.25,
            object2x = FALSE,
            report = TRUE ),
        infoName = "PT Single Asset Barrier Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Barrier.4 =
function()
{   # A function implemented by Diethelm Wuertz

    # Two Asset Barrier Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S1, S2, X, H, Time, r, b1, b2, 
        sigma1, sigma2, rho, object2x, report) {
        object <<- TwoAssetBarrierOption(TypeFlag, S1, S2, X, H, 
            Time, r, b1, b2, sigma1, sigma2, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "puo", 
            S1 = 100, 
            S2 = 100, 
            X = 110,  
            H = 105, 
            Time = 0.5, 
            r = 0.08, 
            b1 = 0.08, 
            b2 = 0.08, 
            sigma1 = 0.2, 
            sigma2 = 0.2, 
            rho = -0.5,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Two Asset Barrier Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Barrier.5 =
function()
{   # A function implemented by Diethelm Wuertz

    # PT Two Asset Barrier Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S1, S2, X, H, time1, Time2, r, b1, 
        b2, sigma1, sigma2, rho, object2x, report) {
        object <<- PTTwoAssetBarrierOption(TypeFlag, S1, S2, X, H, 
            time1, Time2, r, b1, b2, sigma1, sigma2, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "pdo", 
            S1 = 100, 
            S2 = 100, 
            X = 100, 
            H = 85, 
            time1 = 0.5, 
            Time2 = 1, 
            r = 0.1, 
            b1 = 0.1, 
            b2 = 0.1, 
            sigma1 = 0.25, 
            sigma2 = 0.30, 
            rho = -0.5,
            object2x = FALSE,
            report = TRUE ),
        infoName = "PT Two Asset Barrier Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Barrier.6 =
function()
{   # A function implemented by Diethelm Wuertz

    # Look Barrier Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, H, time1, Time2, r, b, 
        sigma, object2x, report) {
        object <<- LookBarrierOption(TypeFlag, S, X, H, time1, 
            Time2, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "cuo", 
            S = 100, 
            X = 100, 
            H = 130, 
            time1 = 0.25, 
            Time2 = 1, 
            r = 0.1, 
            b = 0.1, 
            sigma = 0.15,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Look Barrier Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Barrier.7 =
function()
{   # A function implemented by Diethelm Wuertz

    # Discrete Barrier Option:
    helpTopic <<- ""
    myFunction = function(S, H, sigma, dt, object2x, report) {
        object <<- DiscreteBarrierOption(S, H, 
            sigma, dt)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            S = 100, 
            H = 105, 
            sigma = 0.25, 
            dt = 0.1,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Discrete Barrier Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Barrier.8 =
function()
{   # A function implemented by Diethelm Wuertz

    # Soft Barrier Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, L, U, Time, r, b, sigma, 
        object2x, report) {
        object <<- SoftBarrierOption(TypeFlag, S, X, L, U, Time, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "cdo", 
            S = 100, 
            X = 100, 
            L = 70, 
            U = 95, 
            Time = 0.5, 
            r = 0.1, 
            b = 0.05, 
            sigma = 0.20,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Soft Barrier Option" )
}


################################################################################
# Binary Options


.fOptions.Binary.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Gap Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X1, X2, Time, r, b, sigma, 
        object2x, report) {
        object <<- GapOption(TypeFlag, S, X1, X2, Time, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 50, 
            X1 = 50, 
            X2 = 57, 
            Time = 0.5, 
            r = 0.09, 
            b = 0.09, 
            sigma = 0.20,
            object2x = FALSE,
            report = TRUE ),
        infoName = "GapOption" )
}


# ------------------------------------------------------------------------------


.fOptions.Binary.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Cash Or Nothing Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, K, Time, r, b, sigma, 
        object2x, report) {
        object <<- CashOrNothingOption(TypeFlag, S, X, K, Time, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "p", 
            S = 100, 
            X = 80, 
            K = 10, 
            Time = 0.75, 
            r = 0.06, 
            b = 0, 
            sigma = 0.35,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Cash Or Nothing Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Binary.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # Two Asset Cash Or Nothing Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S1, S2, X1, X2, K, Time, r, b1,
        b2, sigma1, sigma2, rho, object2x, report) {
        object <<- TwoAssetCashOrNothingOption(TypeFlag, S1, S2, X1, 
            X2, K, Time, r, b1, b2, sigma1, sigma2, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S1 = 100, 
            S2 = 100, 
            X1 = 110, 
            X2 = 90, 
            K = 10, 
            Time = 0.5, 
            r = 0.10, 
            b1 = 0.05, 
            b2 = 0.06, 
            sigma1 = 0.20, 
            sigma2 = 0.25, 
            rho = 0.50,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Two Asset Cash Or Nothing Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Binary.4 =
function()
{   # A function implemented by Diethelm Wuertz
 
    # Asset Or Nothing Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma, 
        object2x, report) {
        object <<- AssetOrNothingOption(TypeFlag, S, X, Time, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "p", 
            S = 70, 
            X = 65, 
            Time = 0.5, 
            r = 0.07, 
            b = 0.02, 
            sigma = 0.27,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Asset Or Nothing Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Binary.5 =
function()
{   # A function implemented by Diethelm Wuertz

    # Super Share Option:
    helpTopic <<- ""
    myFunction = function(S, XL, XH, Time, r, b, sigma, object2x, report) {
        object <<- SuperShareOption(S, XL, XH, Time, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            S = 100, 
            XL = 90, 
            XH = 110, 
            Time = 0.25, 
            r = 0.10, 
            b = 0, 
            sigma = 0.20,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Super Share Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Binary.6 =
function()
{   # A function implemented by Diethelm Wuertz

    # Binary Barrier Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, H, K, Time, r, b, sigma, 
        object2x, report) {
        object <<- BinaryBarrierOption(TypeFlag, S, X, H, K, Time, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "6", 
            S = 95, 
            X = 102, 
            H = 100, 
            K = 15, 
            Time = 0.5, 
            r = 0.1, b = 0.1, 
            sigma = 0.20,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Binary Barrier Option" )
}


################################################################################
# Asian Options


.fOptions.Asian.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Geometric Average Rate Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma, object2x, 
        report) {
        object <<- GeometricAverageRateOption(TypeFlag, S, X, Time, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "p", 
            S = 80, 
            X = 85, 
            Time = 0.25, 
            r = 0.05, 
            b = 0.08, 
            sigma = 0.20,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Geometric Average Rate Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Asian.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Turnbull Wakeman Asian Approx Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, SA, X, Time, time, tau, r, 
        b, sigma, object2x, report) {
        object <<- TurnbullWakemanAsianApproxOption(TypeFlag, S, SA, 
            X, Time, time, tau, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "p", 
            S = 90, 
            SA = 88, 
            X = 95,  
            Time = 0.50, 
            time = 0.25, 
            tau = 0.0, 
            r = 0.07, 
            b = 0.02, 
            sigma = 0.25,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Turnbull Wakeman Asian Approx Option" )
}


# ------------------------------------------------------------------------------


.fOptions.Asian.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # Levy Asian Approx Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, SA, X, Time, time, r, b, sigma, 
        object2x, report) {
        object <<- LevyAsianApproxOption(TypeFlag, S, SA, X, Time,  
            time, r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            SA = 100, 
            X = 105, 
            Time = 0.75, 
            time = 0.50, 
            r = 0.10, 
            b = 0.05, 
            sigma = 0.15,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Levy Asian Approx Option" )
}


################################################################################
# Currency Translated Options


.fOptions.CurrencyTranslated.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # FE In Domestic FX Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma, 
        object2x, report) {
        object <<- FEInDomesticFXOption(TypeFlag, S, X, Time, 
            r, b, sigma)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 100, 
            Time = 1, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "FE In Domestic FX Option" )
}


# ------------------------------------------------------------------------------


.fOptions.CurrencyTranslated.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Quanto Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, Ep, X, Time, r, rf, q,
        sigmaS, sigmaE, rho, object2x, report) {
        object <<- QuantoOption(TypeFlag, S, Ep, X, Time, r, rf, q,
            sigmaS, sigmaE, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            Ep = 1.5, 
            X = 105, 
            Time = 0.5, 
            r = 0.08, 
            rf = 0.05, 
            q = 0.04, 
            sigmaS = 0.2, 
            sigmaE = 0.10, 
            rho = 0.30,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Quanto Option" )
}


# ------------------------------------------------------------------------------


.fOptions.CurrencyTranslated.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # Equity Linked FX Option:
    helpTopic <<- ""
    myFunction = function(TypeFlag, E, S, X, Time, r, rf, q, sigmaS,
        sigmaE, rho, object2x, report) {
        object <<- EquityLinkedFXOption(TypeFlag, E, S, X, Time, r, 
            rf, q, sigmaS, sigmaE, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "p", 
            E = 1.5, 
            S = 100, 
            X = 1.52, 
            Time = 0.25, 
            r = 0.08, 
            rf = 0.05, 
            q = 0.04, 
            sigmaS = 0.20, 
            sigmaE = 0.12, 
            rho = -0.40,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Equity Linked FX Option" )
}


# ------------------------------------------------------------------------------


.fOptions.CurrencyTranslated.4 =
function()
{   # A function implemented by Diethelm Wuertz

    # Takeover FX Option:
    helpTopic <<- ""
    myFunction = function(V, B, E, X, Time, r, rf, sigmaV, sigmaE, 
        rho, object2x, report) {
        object <<- TakeoverFXOption(V, B, E, X, Time, r, rf,  
            sigmaV, sigmaE, rho)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            V = 100, 
            B = 100, 
            E = 1.5, 
            X = 1.55, 
            Time = 1, 
            r = 0.08, 
            rf = 0.06, 
            sigmaV = 0.20, 
            sigmaE = 0.25, 
            rho = 0.1,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Takeover FX Option" )
}


################################################################################
# HestonNandiGarchFit


.fOptions.HestonNandi.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Simulate HN-GARCH Process
    helpTopic <<- ""
    myFunction = function(lambda, omega, alpha, beta, gamma, rf, 
        object2x, report) {
        model = list(lambda = lambda, omega = omega, alpha = alpha, 
            beta = beta, gamma = gamma, rf = rf)
        object <<- hngarchSim(model = model, n = n, innov = NULL, 
            n.start = 100, start.innov = NULL, rand.gen = rnorm)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            lambda = 4,
            omega = 0.0008,
            alpha = 0.00006,
            beta = 0.3,
            gamma = 0,
            rf = 0,
            n = 1000,
            object2x = TRUE,
            report = TRUE ),
        infoName = "" )
}


# ------------------------------------------------------------------------------


.fOptions.HestonNandi.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Fit HN-GARCH:
    helpTopic <<- ""
    myFunction = function(series, lambda, omega, alpha, beta, gamma, rf,
        symmetric, object2x, report) {
        x = tkEval(series)
        omega = eval(parse(text = omega))
        alpha = eval(parse(text = alpha))
        model = list(lambda = lambda, omega = omega, alpha = alpha,
            beta = beta, gamma = gamma, rf = rf)
        object <<- hngarchFit(x = x, model = model, symmetric = symmetric, 
            trace = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            lambda = -0.5,
            omega = "var(x)",
            alpha = "0.1*var(x)",
            beta = 0.1,
            gamma = 0,
            rf = 0,
            symmetric = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "HN GARCH Fit" )
}


# ------------------------------------------------------------------------------


.fOptions.HestonNandi.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # ... Print Summary Report:
    helpTopic <<- ""
    myFunction = function(fit, object2x, report) {
        fit = eval(parse(text = "fit"))
        tkOutput(capture.output(summary(fit))) }
    tkExecute(
        fun = myFunction,
        params = list(
            object = "fit",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Summary Report")
}


# ------------------------------------------------------------------------------


.fOptions.HestonNandi.4 =
function()
{   # A function implemented by Diethelm Wuertz

    # ... Print Statistics:
    helpTopic <<- ""
    myFunction = function(model, object2x, report) {
        model = eval(parse(text = model))
        object <<- hngarchStats(model) 
        tkTitle("Heston-Nandi Statistics")
        output = t(as.data.frame(object))
        colnames(output) = "Statistic"
        tkOutput(capture.output(output))
        tkDescription()
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            model = "fit$model",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Statistics" )
}


################################################################################
# Heston Nandi Options


.fOptions.C2Cmd.1 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    myFunction = function(x, object2x, report) {
        object <<- NA }
    tkExecute(
        fun = myFunction,
        params = list(
            x = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Heston Nandi Option" )
}


# ------------------------------------------------------------------------------


.fOptions.C2Cmd.2 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    myFunction = function(x, object2x, report) {
        object <<- NA }
    tkExecute(
        fun = myFunction,
        params = list(
            x = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Heston Nandi Option" )
}


# ------------------------------------------------------------------------------


.fOptions.C2Cmd.3 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    myFunction = function(x, object2x, report) {
        object <<- NA }
    tkExecute(
        fun = myFunction,
        params = list(
            x = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Heston Nandi Option" )
}


# ------------------------------------------------------------------------------


.fOptions.C2Cmd.4 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    myFunction = function(x, object2x, report) {
        object <<- NA }
    tkExecute(
        fun = myFunction,
        params = list(
            x = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Heston Nandi Option" )
}


################################################################################
# Low Discrepancy Sequences


.fOptions.LowDiscrepancy.1 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    tkExecute(
        fun = runif.pseudo,
        params = list(
            n = 100, 
            dimension = 5, 
            init = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Uniform Pseudo Random Numbers" )
}


# ------------------------------------------------------------------------------


.fOptions.LowDiscrepancy.2 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    tkExecute(
        fun = rnorm.pseudo,
        params = list(
            n = 100, 
            dimension = 5, 
            init = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Normal Pseudo Random Numbers" )
}


# ------------------------------------------------------------------------------


.fOptions.LowDiscrepancy.3 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    tkExecute(
        fun = runif.halton,
        params = list(
            n = 100, 
            dimension = 5, 
            init = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Uniform Halton Numbers" )
}


.fOptions.LowDiscrepancy.4 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    tkExecute(
        fun = rnorm.halton,
        params = list(
            n = 100, 
            dimension = 5, 
            init = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Normal Halton Numbers" )
}


# ------------------------------------------------------------------------------


.fOptions.LowDiscrepancy.5 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    tkExecute(
        fun = runif.sobol,
        params = list(
            n = 100, 
            dimension = 5, 
            init = TRUE,
            scrambling = 0,
            seed = 4711,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Uniform Sobol Numbers" )
}


# ------------------------------------------------------------------------------


.fOptions.LowDiscrepancy.6 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    tkExecute(
        fun = rnorm.sobol,
        params = list(
            n = 100, 
            dimension = 5, 
            init = TRUE,
            scrambling = 0,
            seed = 4711,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Normal Sobol Numbers" )
}


################################################################################
# Monte Carlo Options


.fOptions.MonteCarlo.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Plain Vanilla Payoff with Normal Innovations
    helpTopic <<- ""  
    TypeFlag <<- S <<- X <<- Time <<- r <<- b <<- sigma <<- delta.t <<- NA
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma,
        delta.t, pathLength, mcSteps, mcLoops, antithetic, object2x, report) {
        TypeFlag <<- TypeFlag
        S <<- S
        X <<- X
        Time <<- Time
        r <<- r
        b <<- b 
        sigma <<- sigma
        delta.t <<- delta.t
        .normalInnovations <<- function(mcSteps, pathLength, init) {
            # Create Normal Sobol Innovations:
            innovations = rnorm.pseudo(mcSteps, pathLength, init)
            # Return Value:
            innovations }
        .wienerPath <<- function(eps) { 
            # Note, the option parameters must be globally defined!
            # Generate the Paths:
            path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
            # Return Value:
            path }
        .plainVanillaPayoff <<- function(path) { 
            # Note, the option parameters must be globally defined!
            # Compute the Call/Put Payoff Value:
            ST = S*exp(sum(path))
            if (TypeFlag == "c") payoff = exp(-r*Time)*max(ST-X, 0)
            if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-ST)
            # Return Value:
            payoff }
        object <<- MonteCarloOption(
            delta.t = delta.t, pathLength = pathLength,
            mcSteps = mcSteps, mcLoops = mcLoops, init = TRUE, 
            innovations.gen = .normalInnovations, path.gen = .wienerPath, 
            payoff.calc = .plainVanillaPayoff, antithetic = antithetic, 
            standardization = FALSE, trace = TRUE) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 100, 
            Time = 0.08333333, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.40,
            delta.t = 0.002777778, 
            pathLength = 30, 
            mcSteps = 5000, 
            mcLoops = 50, 
            antithetic = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Monte Carlo Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MonteCarlo.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Plain Vanilla Payoff with Sobol Innovations
    helpTopic <<- ""
    TypeFlag <<- S <<- X <<- Time <<- r <<- b <<- sigma <<- delta.t <<- NA
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma,
        delta.t, pathLength, mcSteps, mcLoops, antithetic, object2x, report) {
        TypeFlag <<- TypeFlag
        S <<- S
        X <<- X
        Time <<- Time
        sigma <<- sigma
        r <<- r
        b <<- b 
        .sobolInnovations <<- function(mcSteps, pathLength, init) {
            # Create Normal Sobol Innovations:
            innovations = rnorm.sobol(mcSteps, pathLength, init, 
                scrambling = 2, seed = 4711)
            # Return Value:
            innovations }
        .wienerPath <<- function(eps) { 
            # Note, the option parameters must be globally defined!
            # Generate the Paths:
            path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
            # Return Value:
            path }
        .plainVanillaPayoff <<- function(path) { 
            # Note, the option parameters must be globally defined!
            # Compute the Call/Put Payoff Value:
            ST = S*exp(sum(path))
            if (TypeFlag == "c") payoff = exp(-r*Time)*max(ST-X, 0)
            if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-ST)
            # Return Value:
            payoff }
        object <<- MonteCarloOption(
            delta.t = delta.t, pathLength = pathLength, 
            mcSteps = mcSteps, mcLoops = mcLoops, init = TRUE, 
            innovations.gen = .sobolInnovations, 
            path.gen = .wienerPath, 
            payoff.calc = .plainVanillaPayoff, 
            antithetic = antithetic, 
            standardization = FALSE, trace = TRUE) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 100, 
            Time = 0.08333333, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.40,
            delta.t = 0.002777778, 
            pathLength = 30, 
            mcSteps = 5000, 
            mcLoops = 50, 
            antithetic = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "MC Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MonteCarlo.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # Arithmetic Asian Payoff with Normal Innovations
    helpTopic <<- ""
    TypeFlag <<- S <<- X <<- Time <<- r <<- b <<- sigma <<- delta.t <<- NA
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma,
        delta.t, pathLength, mcSteps, mcLoops, antithetic, object2x, report) {
        TypeFlag <<- TypeFlag
        S <<- S
        X <<- X
        Time <<- Time
        sigma <<- sigma
        r <<- r
        b <<- b 
        .normalInnovations <<- function(mcSteps, pathLength, init) {
            # Create Normal Sobol Innovations:
            innovations = rnorm.pseudo(mcSteps, pathLength, init)
            # Return Value:
            innovations }
        .wienerPath <<- function(eps) { 
            # Note, the option parameters must be globally defined!
            # Generate the Paths:
            path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
            # Return Value:
            path }
        .arithmeticAsianPayoff = function(path) { 
            # Note, the option parameters must be globally defined!
            # Compute the Call/Put Payoff Value:
            SM = mean(S*exp(cumsum(path)))
            if (TypeFlag == "c") payoff = exp(-r*Time)*max(SM-X, 0)
            if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-SM)
            # Return Value:
            payoff }
        object <<- MonteCarloOption(
            delta.t = delta.t, pathLength = pathLength, 
            mcSteps = mcSteps, mcLoops = mcLoops, init = TRUE, 
            innovations.gen = .normalInnovations, 
            path.gen = .wienerPath, 
            payoff.calc = .asianPayoff, 
            antithetic = antithetic, 
            standardization = FALSE, trace = TRUE) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 100, 
            Time = 0.08333333, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.40,
            delta.t = 0.002777778, 
            pathLength = 30, 
            mcSteps = 5000, 
            mcLoops = 50, 
            antithetic = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "MC Option" )
}


# ------------------------------------------------------------------------------


.fOptions.MonteCarlo.4 =
function()
{   # A function implemented by Diethelm Wuertz

    # Arithmetic Asian Payoff with Sobol Innovations
    helpTopic <<- ""
    TypeFlag <<- S <<- X <<- Time <<- r <<- b <<- sigma <<- delta.t <<- NA
    myFunction = function(TypeFlag, S, X, Time, r, b, sigma,
        delta.t, pathLength, mcSteps, mcLoops, antithetic, object2x, report) {
        TypeFlag <<- TypeFlag
        S <<- S
        X <<- X
        Time <<- Time
        sigma <<- sigma
        r <<- r
        b <<- b 
        .sobolInnovations <<- function(mcSteps, pathLength, init) {
            # Create Normal Sobol Innovations:
            innovations = rnorm.sobol(mcSteps, pathLength, init, 
                scrambling = 2, seed = 4711)
            # Return Value:
            innovations }
        .wienerPath <<- function(eps) { 
            # Note, the option parameters must be globally defined!
            # Generate the Paths:
            path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
            # Return Value:
            path }
        .arithmeticAsianPayoff = function(path) { 
            # Note, the option parameters must be globally defined!
            # Compute the Call/Put Payoff Value:
            SM = mean(S*exp(cumsum(path)))
            if (TypeFlag == "c") payoff = exp(-r*Time)*max(SM-X, 0)
            if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-SM)
            # Return Value:
            payoff }
        object <<- MonteCarloOption(delta.t = delta.t, pathLength = 
            pathLength, mcSteps = mcSteps, mcLoops = mcLoops, init = TRUE, 
            innovations.gen = sobolInnovations, path.gen = wienerPath, 
            payoff.calc = plainVanillaPayoff, antithetic = antithetic, 
            standardization = FALSE, trace = TRUE, scrambling = 2, 
            seed = 4711) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 100, 
            Time = 0.08333333, 
            r = 0.10, 
            b = 0.10, 
            sigma = 0.40,
            delta.t = 0.002777778, 
            pathLength = 30, 
            mcSteps = 5000, 
            mcLoops = 50, 
            antithetic = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "MC Option" )
}


################################################################################
# Exponential Brownian Motion


.fOptions.ExponentialBM.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Exponential Brownian Motion
    helpTopic <<- ""
    tkTitle("Exponential Brownian Motion")
    tkOutput("\tSorry, not yet implemented!\n")
}


################################################################################
# Gamma Functions


.fOptions.Gamma.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Plot Gamma Function:
    helpTopic <<- ""
    myFunction = function(range, by, object2x, report) {
        n = round(range, 0)
        par(mfrow = c(1, 1), cex = 0.7)
        plot(x = 0, y = 0, xlim = c(-n, n), ylim = c(-n, n), 
            type = "n", col = "steelblue", xlab = "x", 
            ylab = "Gamma", main = "Gamma Function")
        x = seq(by, 1-by, by = by)
        for (i in 1:n) lines(x-i, gamma(x-i), col = "steelblue")
        lines(n*x, gamma(n*x), col = "steelblue")
        abline(v = 0)
        abline(h = 0)
        for (i in 1:n) lines(x-i, 1/gamma(x-i), col = "steelblue", lty = 2)
        lines(n*x, 1/gamma(n*x), col = "steelblue", lty = 2)
        for (i in 1:n) abline(v = -i, lty = 3)
        x = seq(from = -range, to = range, by = by)
        object <<- data.frame(x = x, gamma = gamma(x), 
            gammaInv = 1/gamma(x)) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            range = 4,
            by = 0.001,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Gamma Function" )
}


# ------------------------------------------------------------------------------


.fOptions.Gamma.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Plot Psi Function:
    helpTopic <<- ""
    myFunction = function(range, by, object2x, report) {      
        par(mfrow = c(1, 1), cex = 0.7)
        n = round(range, 0)
        plot(x = 0, y = 0, xlim = c(-n, n), ylim = c(-n, n), 
            type = "n", col = "steelblue", xlab = "x", 
            ylab = "Psi", main = "Psi Function")
        x = seq(by, 1-by, by = by)
        for (i in 1:n) lines(x-i, Psi(x-i), col = "steelblue")
        lines(n*x, Psi(n*x), col = "steelblue")
        for (i in 1:n) abline(v = -i, lty = 3)
        abline(v = 0)
        abline(h = 0)
        x = seq(from = -range, to = range, by = by)
        object <<- data.frame(x = x, gamma = Psi(x)) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            range = 4,
            by = 0.001,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Psi Function" )
}


# ------------------------------------------------------------------------------


.fOptions.Gamma.3 =
function()
{   # A function implemented by Diethelm Wuertz

    # Plot Complex Gamma Function:
    helpTopic <<- ""
    myFunction = function(range, by, col, nlevels, object2x, report) {            
        col = eval(parse(text = col))
        par(mfrow = c(2, 1), cex = 0.7)
        n = round(range, 0)
        N = ceiling(2*n/by)
        x = y = seq(-n, n, length = N)
        z = 0 * outer(x, x)
        for ( i in 1:N )
            for ( j in 1:N )
                z[i, j] = cgamma(complex(real = x[i], imag = y[j]))         
        Z = Re(z)   
        image(x, y, Z, col = col)
        title("Re - Complex Gamma Function")
        abline(v = -n)
        abline(h = n)
        contour(x, y, Z, nlevels = nlevels, add = TRUE)
        Z = Im(z)   
        image(x, y, Z, col = col)
        title("Im - Complex Gamma Function")
        abline(v = -n)
        abline(h = n)
        contour(x, y, Z, nlevels = nlevels, add = TRUE)
        x = seq(from = -range, to = range, by = by)
        object <<- data.frame(x = x, y = y, Re = Re(z), Im = Im(z)) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            range = 4,
            by = 0.05,
            col = "rainbow(256)",
            nlevels = 50,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Complex Gamma Function" )
}


################################################################################
# Hypergeometric Functions


.fOptions.Hypergeometric.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Menu:
    helpTopic <<- ""
    tkTitle("Kummer Function Slider")
    kummerSlider()
}
    

# ------------------------------------------------------------------------------

    
.fOptions.Hypergeometric.2 =
function()
{   # A function implemented by Diethelm Wuertz

    # Menu:
    helpTopic <<- ""
    tkTitle("Whittaker Function Slider")
    whittakerSlider()

}


################################################################################
# Bessel Functions


.fOptions.Bessel.1 =
function()
{   # A function implemented by Diethelm Wuertz

    # Menu:
    helpTopic <<- ""
    tkTitle("Bessel Function Slider")
    besselSlider()
}


################################################################################
# EBM Asian Options


.fOptions.EBMAsian.1 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, sigma, method, 
        object2x, report) {
        object <<- MomentMatchedAsianOption(TypeFlag, S, X, Time, r, 
            sigma, method) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 100, 
            Time = 1, 
            r = 0.09, 
            sigma = 0.30,
            method = "LN",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Moment Matched Asian Option" )
}


# ------------------------------------------------------------------------------


.fOptions.EBMAsian.2 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    myFunction = function(x, Time, r, sigma, method, object2x, report) {
        x = eval(parse(text = x))
        object <<- MomentMatchedAsianDensity(x = x, Time, r, 
            sigma, method) 
        par(mfrow = c(1,1), cex = 0.7)
        plot(x, object, type = "b", pch = 19)
        title(main = "Moment Matched Asian Density")
        title(main = paste("\n\nMethod:", method))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            x = "seq(from = 0, to = 2.5, length = 100)",  
            Time = 1, 
            r = 0.09, 
            sigma = 0.30,
            method = "LN",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Moment Matched Asian Density" )
}


# ------------------------------------------------------------------------------


.fOptions.EBMAsian.2 =
function()
{   # A function implemented by Diethelm Wuertz

    #
    helpTopic <<- ""
    myFunction = function(TypeFlag, S, X, Time, r, sigma, method, 
        object2x, report) {
        object <<- GramCharlierAsianOption(TypeFlag, S, X, Time, r, 
            sigma, method) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            TypeFlag = "c", 
            S = 100, 
            X = 100, 
            Time = 1, 
            r = 0.09, 
            sigma = 0.30,
            method = "LN",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Gram Charlier Asian Option" )
}     


################################################################################

