
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
# FUNCTION:                 REGRESSION TERM PLOTS
#  .termPlot                 Line Plot          
#  .termPersp                Perspective Plot         
#  .termContour              Contour Plot
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TermPlots, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.termPlot = 
function()
{    
    # Requirements:
    require(MASS)
    require(polspline)
    
    # Simulate Data - a data frame:
    DATA = regSim(model = "GAM3", n = 100)
    head(DATA)
    class(DATA)
    
    # Convert to a timeSeries object:
    DATATS = as.timeSeries(DATA)
    head(DATATS)
    class(DATATS)
    
    # Fit:
    LM    = regFit(Y ~ 1 + X1 + X2 + X3, data = DATATS, use = "lm") 
    RLM   = regFit(Y ~ 1 + X1 + X2 + X3, data = DATATS, use = "rlm") 
    AM    = regFit(Y ~ 1 + s(X1)+s(X2)+s(X3),   DATATS, use = "am") 
    PPR   = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "ppr") 
    PPR4  = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "ppr", nterms = 4) 
    MARS  = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "mars") 
    PMARS = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "polymars") 
    NNET  = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "nnet") 
    NNET6 = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "nnet", size = 6)    
    
    # Term Plot:
    par(ask = FALSE) 
    par(mfrow = c(1, 1))
    .termPlot(LM)
    .termPlot(RLM)
    .termPlot(AM)
    .termPlot(PPR)
    .termPlot(MARS)
    .termPlot(PMARS)
    .termPlot(NNET)
    
    # 
    par(ask = FALSE) 
    par(mfrow = c(1, 1))
    .termPlot(LM, terms = "X1")
    .termPlot(RLM, terms = "X1")
    .termPlot(AM, terms = "X1")
    .termPlot(PPR, terms = "X1")
    .termPlot(PPR4, terms = "X1")
    .termPlot(MARS, terms = "X1")
    .termPlot(PMARS, terms = "X1")
    .termPlot(NNET, terms = "X1")
    .termPlot(NNET6, terms = "X1")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.termPersp = 
function()
{    
    # Requirements:
    require(MASS)
    require(polspline)
    
    # Simulate Data - a data frame:
    DATA = regSim(model = "GAM3", n = 100)
    head(DATA)
    class(DATA)
    # Convert to a timeSeries object:
    DATATS = as.timeSeries(DATA)
    head(DATATS)
    class(DATATS)
    
    # Fit:
    LM    = regFit(Y ~ 1 + X1 + X2 + X3, data = DATATS, use = "lm") 
    RLM   = regFit(Y ~ 1 + X1 + X2 + X3, data = DATATS, use = "rlm") 
    AM    = regFit(Y ~ 1 + s(X1)+s(X2)+s(X3),   DATATS, use = "am") 
    PPR   = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "ppr") 
    PPR4  = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "ppr", nterms = 4) 
    MARS  = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "mars") 
    PMARS = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "polymars") 
    NNET  = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "nnet") 
    NNET6 = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "nnet", size = 6)        
    
    # Bivariate Perspective Term Plot: 
    par(ask = FALSE)
    par(mfrow = c(1, 1))
    .termPersp(LM,    terms = c("X1", "X2"))
    .termPersp(RLM,   terms = c("X1", "X2"))
    .termPersp(AM,    terms = c("X1", "X2"))
    .termPersp(PPR,   terms = c("X1", "X2"))
    .termPersp(PPR4,  terms = c("X1", "X2"))
    .termPersp(MARS,  terms = c("X1", "X2"))
    .termPersp(PMARS, terms = c("X1", "X2"))
    .termPersp(NNET,  terms = c("X1", "X2"))
    .termPersp(NNET6, terms = c("X1", "X2"))  
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.termContour = 
function()
{    
    # Requirements:
    require(MASS)
    require(polspline)
    
    # Simulate Data - a data frame:
    DATA = regSim(model = "GAM3", n = 100)
    head(DATA)
    class(DATA)
    # Convert to a timeSeries object:
    DATATS = as.timeSeries(DATA)
    head(DATATS)
    class(DATATS)
    
    # Fit:
    LM    = regFit(Y ~ 1 + X1 + X2 + X3, data = DATATS, use = "lm") 
    RLM   = regFit(Y ~ 1 + X1 + X2 + X3, data = DATATS, use = "rlm") 
    AM    = regFit(Y ~ 1 + s(X1)+s(X2)+s(X3),   DATATS, use = "am") 
    PPR   = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "ppr") 
    PPR4  = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "ppr", nterms = 4) 
    MARS  = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "mars") 
    PMARS = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "polymars") 
    NNET  = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "nnet") 
    NNET6 = regFit(Y ~ X1 + X2 + X3,     data = DATATS, use = "nnet", size = 6)        
        
    # Bivariate Contour Term Plot:
    par(ask = FALSE)
    par(mfrow = c(1, 1))
    .termContour(LM,    terms = c("X1", "X2"))
    .termContour(RLM,   terms = c("X1", "X2"))
    .termContour(AM,    terms = c("X1", "X2"))
    .termContour(PPR,   terms = c("X1", "X2"))
    .termContour(PPR4,  terms = c("X1", "X2"))
    .termContour(MARS,  terms = c("X1", "X2"))
    .termContour(PMARS, terms = c("X1", "X2"))
    .termContour(NNET,  terms = c("X1", "X2"))
    .termContour(NNET6, terms = c("X1", "X2"))
       
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.termComparison = 
function()
{    
    # Requirements:
    require(MASS)
    require(polspline)
    
    # Simulate Data - a data frame:
    DATA = regSim(model = "GAM3", n = 100)
    head(DATA)
    class(DATA)
    # Convert to a timeSeries object:
    DATATS = as.timeSeries(DATA)
    head(DATATS)
    class(DATATS)    
 
    # Comparison:
    par(ask = FALSE)
    par(mfrow = c(1, 1))
    .termPlot(LM)
    lm = lm(Y ~ X1 + X2 + X3, DATA)
    termplot(lm, rug = TRUE, partial.resid = TRUE, se = TRUE, pch = 19, 
        main = "LM", ask = par("ask"))
    .termPlot(AM)
    am = gam(formula = Y ~ s(X1) + s(X2) + s(X3), data = DATA) 
    for (s in 1:3) { 
        plot(am, residuals = residuals(am), se = TRUE, 
            main = "AM", cex = 0.7, select = s, pch = 19); grid() 
    }
    
    # Return Value:
    return()
}


################################################################################


if (FALSE) {
    require(RUnit)
    testResult = runTestFile("C:/Rmetrics/SVN/trunk/fMultivar/test/runit2C.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################
    
