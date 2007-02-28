
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file 


################################################################################
# FUNCTION:                  UTILITY FUNCTIONS:
#  .ellipticalParam           Sets Default parameters for an elliptical copula
#  .ellipticalRange           Returns the range of valid rho values
#  .ellipticalCheck           Checks if rho is in the valid range
# FUNCTION:                  ELLIPTICAL GENERATOR AND RELATED FUNCTIONS:
#  gfunc                      Generator function for elliptical distributions
#  gfuncSlider                Slider for generator, density and probability
#  .pelliptical               Univariate elliptical distribution probability
#  .delliptical               Univariate elliptical distribution density
#  .qelliptical               Univariate elliptical distribution quantiles
#  .qlogistic                 Fast tabulated logistic quantile function
#  .qlogisticData             Table generator for logistic quantiles
# FUNCTION:                  ELLIPTICAL COPULAE DEPENDENCE MASURES:
#  ellipticalTau              Computes Kendall's tau for elliptical copulae
#  ellipticalRho              Computes Spearman's rho for elliptical copulae
# FUNCTION:                  ELLIPTICAL COPULAE TAIL COEFFICIENT:
#  ellipticalTailCoeff        Computes tail dependence for elliptical copulae
#  ellipticalTailPlot         Plots tail dependence function
# FUNCTION:                  ELLIPTICAL COPULAE RANDOM DEVIATES:
#  rellipticalCopula          Generates elliptical copula variates
#  rellipticalSlider          Interactive plots of random variates
#  .rnormCopula               Generates normal copula random variate
#  .pnormCopula               Computes normal copula probability
#  .dnormCopula               Computes normal copula density
#  .rcauchyCopula             Generates Cauchy copula random variate
#  .pcauchyCopula             Computes Cauchy copula probability
#  .dcauchyCopula             Computes Cauchy copula density
#  .rtCopula                  Generates Student-t copula random variate
#  .ptCopula                  Computes Student-t copula probability
#  .dtCopula                  Computes Student-t copula density
# FUNCTION:                  ELLIPTICAL COPULAE PROBABILITY:
#  pellipticalCopula          Computes elliptical copula probability
#  pellipticalSlider          Interactive plots of probability
#  .pellipticalCopulaGrid     Fast equidistant grid version
#  .pellipticalCopulaDiag     Fast diagonal cross section version
#  .pellipticalPerspSlider    Interactive perspective plots of probability
#  .pellipticalContourSlider  Interactive contour plots of probability
# FUNCTION:                  ELLIPTICAL COPULAE DENSITY:
#  dellipticalCopula          Computes elliptical copula density 
#  dellipticalSlider          Interactive plots of density
#  .dellipticalCopulaGrid     Fast grid version for elliptical copula density
#  .dellipticalCopula.RUnit   R Unit test for elliptical copula density
#  .dellipticalPerspSlider    Interactive perspective plots of density
#  .dellipticalContourSlider  Interactive contour plots of density
# FUNCTION:                  ELLIPTICAL COPULAE PARAMETER FITTING:
#  ellipticalCopulaSim        Simulates bivariate elliptical copula
#  ellipticalCopulaFit        Fits the paramter of an elliptical copula
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(EllipticalCopulae, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.utilityFunctions = 
function()
{

    # Arguments ?
    args(.ellipticalParam)
    
    # Parameters:
    Type = c("norm", "cauchy", "t", "logistic", "laplace", "kotz", "epower")
    for (type in Type) {
        cat("\n")
        print(unlist(.ellipticalParam(type)))
    }
    
    # Range:
    for (type in Type) {
        cat("\n")
        print(.ellipticalRange(type))
    }

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.generatorFunctions = 
function()
{
    # Arguments ?
    args(gfunc)

    # Call Generator Function - Missing x:
    gfunc(type = "norm")
    gfunc(type = "cauchy")
    gfunc(type = "t")
    gfunc(type = "t", param = 2)
    gfunc(type = "logistic")
    gfunc(type = "laplace")
    gfunc(type = "kotz")
    gfunc(type = "kotz", param = 2)
    gfunc(type = "epower")  
    gfunc(type = "epower", param = c(2, 1))
      
    # Call Generator Function - With specified x:
    gfunc(0:10, type = "norm")
    gfunc(0:10, type = "cauchy")
    gfunc(0:10, type = "t") 
    gfunc(0:10, type = "logistic")
    gfunc(0:10, type = "laplace")
    gfunc(0:10, type = "kotz") 
    gfunc(0:10, type = "epower") 
    
    # Slider:
    # gfuncSlider()
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.dependenceMeasures = 
function()
{
    # Computes Kendall's tau for elliptical copulae
    args(ellipticalTau)
    ellipticalTau(rho = 0.5)
    ellipticalTau(rho = c(-0.5, 0, 0.5))
    
    # Computes Spearman's rho for elliptical copulae
    args(ellipticalRho)
    ellipticalRho(0.5)
    ellipticalRho(rho = c(-0.5, 0, 0.5))
 
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.tailCoefficient = 
function()
{
    # Lower - Upper ----
    
    # Tail Coefficient - Using Default Parameters:
    Type = c("norm", "cauchy", "t") 
    for (type in Type) {
        ans = ellipticalTailCoeff(rho = 0.5, type = type)   
        print(ans)
        cat("\n")
    }
    
    # Plot - Be patient, plotting takes some time ...
    Type = c("norm", "cauchy", "t") 
    for (type in Type) {
        par(mfrow = c(2, 2), cex = 0.7)
        ellipticalTailPlot(type = type)  
        ellipticalTailPlot(type = type, tail = "Lower")   
    }           
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.rCopulae = 
function()
{

    # Random Number Generator:
    R = rellipticalCopula(1000, type = "norm")
    plot(R, pch = 19, col = "steelblue", main = "norm")
    grid()
    
    R = rellipticalCopula(1000, type = "cauchy")
    plot(R, pch = 19, col = "steelblue", main = "cauchy")
    grid()
    
    R = rellipticalCopula(1000, type = "t") 
    plot(R, pch = 19, col = "steelblue", main = "t-default")
    grid()
    
    R = rellipticalCopula(1000, param = c(nu = 3), type = "t")
    plot(R, pch = 19, col = "steelblue", main = "t3")
    grid()
    
    R = rellipticalCopula(1000, param = 3, type = "t")
    plot(R, pch = 19, col = "steelblue", main = "t3")
    grid()
    
    # Try Slider:
    # rellipticalSlider()
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.pCopulae = 
function()
{
       
    # Arguments ?
    args(pellipticalCopula)
    
    # All Types:
    Type = c("norm", "cauchy", "t", "logistic", "laplace", "kotz", "epower")
    
    # Use Default Settings:
    for (type in Type) {
        UV = grid2d()
        p = pellipticalCopula(u = UV, rho = 0.75, type = type, output = "list")   
        persp(p, main = type, theta = -40, phi = 30, col = "steelblue", 
            ps = 9, xlab = "u", ylab = "v", zlab = "C")
    }
        
    # Try Perspective Slider:
    # pellipticalSlider()
    
    # Try Contour Slider:
    # pellipticalSlider("contour")
   
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.dCopulae = 
function()
{  
    # Arguments ?
    args(dellipticalCopula)

    # All Types:
    Type = c("norm", "cauchy", "t", "logistic", "laplace", "kotz", "epower")
    
    # Use Default Settings:
    for (type in Type) {
        UV = grid2d()
        d = dellipticalCopula(u = UV, rho = 0.75, type = type, output = "list")   
        persp(d, main = type, theta = -40, phi = 30, col = "steelblue", 
            ps = 9, xlab = "u", ylab = "v", zlab = "c")
    }
    
    # Try Perspective Slider:
    # dellipticalSlider()
    
    # Try Contour Slider:
    # dellipticalSlider("contour")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.copulaSimFit = 
function()
{  
    # Arguments ?
    args(ellipticalCopulaSim)
    
    # Arguments ?
    args(ellipticalCopulaFit)

    # Normal Copula:
    rho = 0.5
    R = ellipticalCopulaSim(n = 1000, rho = rho)
    R[1:10, ]
    plot(R, pch = 19)
    fit = ellipticalCopulaFit(u = R[,1], v = R[,2])
    fit
    rho - fit$par
    plot(c(-1,1), c(-1,1), xlab = "rho", ylab = "estimate")
    for ( i in 1:100) {
        rho = runif(1, -1, 1)
        R = ellipticalCopulaSim(n = 1000, rho = rho)
        fit = ellipticalCopulaFit(R)
        points(rho, fit$par)
        print(c(rho-fit$par, fit$Rho-fit$par))
    }

    # Cauchy Copula:
    rho = runif(1, -1, 1)
    R = ellipticalCopulaSim(n = 100, rho = rho, type = "cauchy")
    R[1:10, ]
    plot(R, pch = 19)
    ellipticalCopulaFit(R, type = "cauchy")
    rho
    plot(c(-1,1), c(-1,1))
    for ( i in 1:100) {
        rho = runif(1, -1, 1)
        R = ellipticalCopulaSim(n = 1000, rho = rho, type = "cauchy")
        fit = ellipticalCopulaFit(R, type = "cauchy")
        points(rho, fit$par)
        print(c(rho-fit$par, fit$Rho-fit$par))
    }
    
    # Student-t Copula:
    rho = runif(1, -1, 1)
    nu = runif(1, 3, 20)
    print(c(rho, nu))
    R = ellipticalCopulaSim(n = 1000, rho = rho, param = nu, type = "t")
    R[1:10, ]
    plot(R, pch = 19)
    ellipticalCopulaFit(R, type = "t")
    plot(c(-1,1), c(-1,1))
    for ( i in 1:100) {
        rho = runif(1, -1, 1)
        nu = runif(1, 3, 20)
        R = ellipticalCopulaSim(n = 1000, rho = rho, param = nu, type = "t")
        fit = ellipticalCopulaFit(R, type = "t")
        points(rho, fit$par[1])
        print(c(rho, nu, fit$par))
    }
    
    # Return Value:
    return()
    
    

################################################################################


test.parameterSettings = 
function()
{
    # Elliptical Parameter Settings:
    for ( type in 
        c("norm", "cauchy", "t", "logistic", "laplace", "kotz",  "epower") ) {
        print(unlist(.ellipticalParam(type)))
        cat("\n")
    }
      
    # Elliptical Parameter Range:
    for ( type in 
        c("norm", "cauchy", "t", "logistic", "laplace", "kotz",  "epower") ) {
        print(unlist(.ellipticalRange(type)))
        cat("\n")
    }
    
    # Return Value:
    return()
    
    
# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCopulae/test/runit2A.R")
    printTextProtocol(testResult)
}
 

  
################################################################################
   
