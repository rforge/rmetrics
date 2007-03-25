
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
# FUNCTION:                  ELLIPTICAL GENERATOR AND RELATED FUNCTIONS:
#  gfunc                      Generator function for elliptical distributions
#  gfuncSlider                Slider for generator, density and probability
# FUNCTION:                  ELLIPTICAL COPULAE DEPENDENCE MASURES:
#  ellipticalTau              Computes Kendall's tau for elliptical copulae
#  ellipticalRho              Computes Spearman's rho for elliptical copulae
# FUNCTION:                  ELLIPTICAL COPULAE TAIL COEFFICIENT:
#  ellipticalTailCoeff        Computes tail dependence for elliptical copulae
#  ellipticalTailPlot         Plots tail dependence function
# FUNCTION:                  ELLIPTICAL COPULAE RANDOM DEVIATES:
#  rellipticalCopula          Generates elliptical copula variates
#  rellipticalSlider          Interactive plots of random variates
# FUNCTION:                  ELLIPTICAL COPULAE PROBABILITY:
#  pellipticalCopula          Computes elliptical copula probability
#  pellipticalSlider          Interactive plots of probability
# FUNCTION:                  ELLIPTICAL COPULAE DENSITY:
#  dellipticalCopula          Computes elliptical copula density 
#  dellipticalSlider          Interactive plots of density
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


test.gfunc = 
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
    gfunc(x = 0:10, type = "norm")
    gfunc(x = 0:10, type = "cauchy")
    gfunc(x = 0:10, type = "t") 
    gfunc(x = 0:10, type = "logistic")
    gfunc(x = 0:10, type = "laplace")
    gfunc(x = 0:10, type = "kotz") 
    gfunc(x = 0:10, type = "epower") 
    
    # Try Slider:
    gfuncSlider()
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.ellipticalTau = 
function()
{
    # Computes Kendall's tau for elliptical copulae
    args(ellipticalTau)
    ellipticalTau(rho = 0.5)
    ellipticalTau(rho = c(-0.5, 0, 0.5))
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.ellipticalRho = 
function()
{    
    # Computes Spearman's rho for elliptical copulae
    args(ellipticalRho)
    ellipticalRho(0.5)
    ellipticalRho(rho = c(-0.5, 0, 0.5))
 
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.ellipticalTailCoeff = 
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
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.ellipticalTailPlot = 
function()
{    
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


test.rellipticalCopula = 
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


test.pellipticalCopula = 
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


test.dellipticalCopula = 
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
     
    
# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCopulae/test/runit2A.R")
    printTextProtocol(testResult)
}
 

  
################################################################################
   
