
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
# You should have received A copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 INTERNAL USED PLOT FUNCTIONS:
#  .responsesPlot            Returns a response series plot
#  .residualsPlot            Returns a residual series plot
#  .firePlot                 Returns a fitted values vs.residuals plot
#  .acfPlot                  Returns a autocorrelation function plot
#  .pacfPlot                 Returns a partial ACF plot
#  .mrlPlot                  Returns a mean residual life plot
# FUNCTION:                 INTERNAL THREE-DIMENSIONAL PLOT UTILITIES:
#  .circlesPlot              Returns a circles plot indexing 3rd variable
#  .perspPlot                Returns a perspective plot in 2 dimensions
#  .contourPlot              Returns a contour plot in 2 dimensions
# FUNCTION:                 CHARACTER, SYMBOL AND COLOR TABLES:
#  characterTable            Shows a table of character's codes 
#  symbolTable               Shows a table of plot symbols
#  colorTable                Shows a table of plot color codes
# FUNCTION:                 COLOR PALETTES:
#  greyPalette               Creates a grey palette
#  .chcode                   Changes from one to another number system
#  .hex.to.dec               Converts heximal numbers do decimal numbers
#  .dec.to.hex               Converts decimal numbers do heximal numbers
# FUNCTION:                 SLIDER MENU:
#  .sliderMenu               Starts a slider menu
# FUNCTION:                 AKIMA SPLINE INTERPOLATION:
#  .akimaInterpolation       Does Akima Spline Interpolation
#  .interp                   Does Akima Spline Interpolation
# FUNCTION:                 JARQUE-BERA DATA TABLES:
# .jbLM                      Jarque-Bera Lagrange Multiplier Test Data
# .jbALM                     JB Augmented Lagrange Multiplier Test Data
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(fBasicsUtilities, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# CHARACTER, SYMBOL AND COLOR TABLES:


test.tables = 
function()
{
    #  characterTable            Shows a table of character's codes 
    #  symbolTable               Shows a table of plot symbols
    #  colorTable                Shows a table of plot color codes
    
    # Shows a table of character's codes
    characterTable()
    
    # Shows a table of plot symbols
    symbolTable()
    
    # Shows a table of plot color codes
    colorTable()   
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# COLOR PALETTES:


test.palette = 
function()
{
    #  Creates a grey palette 
    greyPalette()   
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# AKIMA SPLINE INTERPOLATION:


test.interp = 
function()
{
    # Data:
    akima = list(
    x = c(
    11.16, 24.20, 12.85, 19.85, 10.35, 24.65, 19.72, 15.91,  0.00, 20.87, 
     6.71,  3.45, 19.99, 14.26, 10.28,  4.51, 17.43, 22.80,  0.00,  7.58, 
    16.70,  6.08,  1.99, 25.00, 14.90,  3.22,  0.00,  9.66,  2.56,  5.22, 
    11.77, 17.25, 15.10, 25.00, 12.13, 25.00, 22.33, 11.52, 14.59, 15.20,  
     7.54,  5.23, 17.32,  2.14,  0.51, 22.69, 25.00,  5.47, 21.67,  3.31),
    y = c(
     1.24, 16.23,  3.06, 10.72,  4.11,  2.40,  1.39,  7.74, 20.00, 20.00, 
     6.26, 12.78,  4.62, 17.87, 15.16, 20.00,  3.46, 12.39,  4.48,  1.98, 
    19.65,  4.58,  5.60, 11.87,  3.12, 16.78,  0.00, 20.00,  3.02, 14.66, 
    10.47, 19.57, 17.19,  3.87, 10.79,  0.00,  6.21,  8.53,  8.71,  0.00, 
    10.69, 10.72, 13.78, 15.03,  8.37, 19.63, 20.00, 17.13, 14.36,  0.13),
    z = c(
    22.15,  2.83, 22.11,  7.97, 22.33, 10.25, 16.83, 15.30, 34.60,  7.54,
    30.97, 41.24, 14.72, 10.74, 21.59, 15.61, 18.60,  5.47, 61.77, 29.87,  
     6.31, 35.74, 51.81,  4.40, 21.70, 39.93, 58.20,  4.73, 50.55, 40.36, 
    13.62,  6.43, 12.57,  8.74, 13.71, 12.00, 10.25, 15.74, 14.81, 21.60, 
    19.31, 26.50, 12.11, 53.10, 49.43,  3.25,  0.60, 28.63,  5.52, 44.08))
    
    .interp.new(akima$x, akima$y, akima$z, linear = FALSE,
        ncp = NULL, extrap = FALSE, duplicate = "median", dupfun = NULL)

    # Interpolation:
    akima.lin = .akima2D(akima$x, akima$y, akima$z)

    Z = mean(akima.lin$z)
    checkSum = 21.70316
    checkEquals(target = Z, current = checkSum, tolerance = 0.00001)
    
    # Plots:
    par(ask = FALSE)
    par(mfrow = c(1, 1))
    image  (akima.lin, add = FALSE)
    contour(akima.lin, add = TRUE)
    points (akima, pch = 19)
     
    # Interpolation:
    .akima2D(akima$x, akima$y, akima$z, interp = "linear", extrap = TRUE)
    .akima2D(akima$x, akima$y, akima$z, interp = "linear", extrap = FALSE)
    .akima2D(akima$x, akima$y, akima$z, interp = "spline", extrap = TRUE)
    .akima2D(akima$x, akima$y, akima$z, interp = "spline", extrap = FALSE)
    
    # Return Value:
    return()    
}

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit1A.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------


# spatial:

    require(akima)
    require(fBasics)
    data(akima)
    akima = as.data.frame(akima) 
 
    
akimaInterp <- 
function(x, ...) 
{
    UseMethod("akimaInterp")   
}


# ------------------------------------------------------------------------------

    
akimaInterp.formula = 
function(formula = z ~ x + y, data, grid = 40, doplot = TRUE, ...)
{
    # Settings:
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    Data <- eval(mf, parent.frame())
    x = Data[, 2]
    y = Data[, 3]
    z = Data[, 1]
    
    # Interpolate:
    Z = akimaInterp.default(x = x, y = y, z = z, 
        grid = grid, doplot = doplot, ...)
    
    # Return Value:
    invisible(Z)
}


akimaInterp.default = 
function(x, y, z, grid = 40, doplot = TRUE, ...)
{
    # Settings:
    if (is.data.frame(x)) x = as.matrix.data.frame(x_)
    if (is.matrix(x)) {
        z = x[, 3]
        y = x[, 2]
        x = x[, 1]
    }
    
    # Interpolate:
    Z = interp(x = x, y = y, z = z, 
        xo = seq(min(x), max(x), length = grid),
        yo = seq(min(y), max(y), length = grid),
        linear = FALSE, extrap = TRUE, duplicate = "median",
        dupfun = NULL, ncp = NULL) 
        
    # Plot:
    if (doplot) {
        image(Z)
        contour(Z, add = TRUE)
    }
    
    # Return Value:
    invisible(Z)
}


akimaInterp(z ~ x + y, data = akima, grid = 100, doplot = TRUE)


krigeInterp.formula = 
function(formula = z ~ x + y, data, grid = 40, doplot = TRUE, ...)
{
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    Data <- eval(mf, parent.frame())
    x = Data[, 2]
    y = Data[, 3]
    z = Data[, 1]
    
    # Interpolate:
    krige = surf.gls(np = 6, covmod = expcov, 
        x = x, y = y, z = z, d = 0.5, alpha = 1)
    Z = prmat(krige, xl=min(x), xu=max(x), yl=min(y), yu=max(y), n = grid)
    
    # Plot:
    if (doplot) {
        image(Z)
        contour(Z, add = TRUE)
    }
    
    # Return Value:
    invisible(Z)
}

krigeInterp(z ~ x + y, data = akima, grid = 100, doplot = TRUE)
    

,
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")