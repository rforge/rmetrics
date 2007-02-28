
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
# FUNCTION:                  ARCHIMEDEAN COPULAE PARAMETER:
#  .archmParam                Sets Default parameters for an Archimedean copula
#  .archmCheck                Checks if alpha is in the valid range
#  .archmRange                Returns the range of valid alpha values
# FUNCTION:                  ARCHIMEDEAN COPULAE PHI GENERATOR:
#  Phi                        Computes Archimedean Phi, inverse and derivatives
#  PhiSlider                  Displays interactively generator function
#  .Phi                       Computes Archimedean generator Phi
#  .Phi0                      Utility Function
#  .PhiFirstDer               Computes first derivative of Phi
#  .PhiSecondDer              Computes second derivative of Phi
#  .invPhi                    Computes inverse of Archimedean generator
#  .invPhiFirstDer            Computes first derivative of inverse Phi
#  .invPhiSecondDer           Computes second derivative of inverse Phi
# FUNCTION:                  ARCHIMEDEAN DENSITY K GENERATOR:
#  Kfunc                      Computes Archimedean Density Kc and its Inverse
#  KfuncSlider                Displays interactively the density and concordance
#  .Kfunc                     Computes Density for Archimedean Copulae
#  .invK                      Computes Inverse of Density
#  .invK2                     Utility Function
#  .ALPHA                     Utility Function
#  .TAU                       Utility Function
#  .RHO                       Utility Function
# FUNCTION                   KENDALL'S TAU AND SPEARMAN'S RHO:
#  archmTau                   Returns Kendall's tau for Archemedean copulae
#  archmRho                   Returns Spearman's rho for Archemedean copulae
#  .archmTauRange              Returns range for Kendall's tau
#  .archm2Tau                  Alternative way to compute Kendall's tau
#  .archmGamma                Returns Gini's gamma for Archimedean copulae
#  .archmTail                  Utility Function
# FUNCTION:                  ARCHIMEDEAN COPULAE TAIL COEFFICIENT:
#  archmTailCoeff       X     Computes tail dependence for Archimedean copulae
#  archmTailPlot        X     Plots Archimedean tail dependence function
# FUNCTION:                  ARCHIMEDEAN COPULAE RANDOM VARIATES:
#  rarchmCopula               Generates Archimedean copula random variates 
#  .r1Copula                  Generates rv's for copulae No 1
#  .r2Copula                  Generates rv's for copulae No 2
# FUNCTION:                  ARCHIMEDEAN COPULAE PROBABILITY:
#  parchmCopula               Computes Archimedean copula probability 
#  parchmSlider               Displays interactively archimedean probability 
#  .parchm1Copula              Utility Function
#  .parchm2Copula              Utility Function
#  .parchmPerspSlider          Utility Function
#  .parchmContourSlider        Utility Function
# FUNCTION:                  ARCHIMEDEAN COPULAE DENSITY:
#  darchmCopula               Computes Archimedean copula density 
#  darchmSlider                Displays interactively archimedean density 
#  .darchm1Copula              Utility Function
#  .darchm2Copula              Utility Function
#  .darchmPerspSlider          Utility Function
#  .darchmContourSlider        Utility Function
# FUNCTION:                  ARCHIMEDEAN COPULAE PARAMETER FITTING:
#  archmCopulaSim             Simulates bivariate elliptical copula
#  archmCopulaFit             Fits the paramter of an elliptical copula
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ArchimedeanCopulae, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# Utility Functions:


test.utilityFunctions = 
function()
{

    # Arguments ?
    args(.archmParam)
    
    # Parameters:
    for (type in 1:22) {
        cat("\n")
        print(unlist(.archmParam(type)))
    }
    
    # Range:
    for (type in 1:22) {
        cat("\n")
        print(.archmRange(type))
    }

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# Generator Function Phi:


test.generatorFunction = 
function()
{
    # Arguments ?
    args(Phi)

    # Call Generator Function Phi:
    for (type in 1:22) {
        print(Phi(0.5, inv = TRUE, deriv = 0))
        cat("\n")
    }
    for (type in 1:22) {
        print(Phi(0.5, inv = TRUE, deriv = 1))
        cat("\n")
    }
    for (type in 1:22) {
        print(Phi(0.5, inv = TRUE, deriv = 2))
        cat("\n")
    }
    
    for (type in 1:22) {
        print(Phi(0.5, inv = FALSE, deriv = 0)) 
        cat("\n")
    }
    for (type in 1:22) {
        print(Phi(0.5, inv = FALSE, deriv = 1))
        cat("\n")
    }
    for (type in 1:22) {
        print(Phi(0.5, inv = FALSE, deriv = 2))
        cat("\n")
    }
    
    # Try Slider:
    # PhiSlider()
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# K Density Function:


test.densityFunction = 
function()
{
    # Arguments ?
    args(Kfunc)
    
    # Call Generator Function Phi:
    for (type in 1:22) {
        print(Kfunc(0.5, inv = TRUE))
        cat("\n")
    }
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
# Dependence Measures:
 
    
test.dependenceMeasures = 
function()
{
    # Arguments ?
    args(archmTau)
    
    # Tau Value:
    for (type in 1:22) {
        print(archmTau(type = type))
        cat("\n")
    }
    
    # Vector Tau Value:
    for (type in 1:22) {
        alpha = .archmParam(type)$param
        print(archmTau(c(alpha, alpha+0.1), type = type))
        cat("\n")
    }
    
    # Tau Range:
    for (type in 1:22) {
        range = .archmTauRange(type)
        print(range) 
        cat("\n")
    }
    
    # Rho Value:
    for (type in 1:22) {
        print(archmRho(type = type))
        cat("\n")
    }
    
    # Vector Rho Value - Method "integrate2d":
    for (type in 1:22) {
        alpha = .archmParam(type)$param
        print(archmRho(c(alpha, alpha+0.1), type = type, 
            method = "integrate2d", error = 1e-5))
        cat("\n")
    }
    
    # Vector Rho Value - Method "adapt":
    # ...
    
    # Return Value:
    return()    
}
    

# ------------------------------------------------------------------------------
# Tail Coefficient:
 
    
test.tailCoefficient = 
function()
{
    # archmTailCoeff  X  Computes tail dependence for Archimedean copulae
    # archmTailPlot   X  Plots Archimedean tail dependence function

    NA
 
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------  
# Copula Random Variates:
 
    
test.rCopula = 
function()
{
    # Random Variates - Check all Types:
    for (type in 1:22) {
        R = rarchmCopula(n = 5, alpha = NULL, type = type)
        print(R)
        cat("\n")
    }
    
    # Try Slider:
    # rarchmSlider()
      
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------  
# Copula Probability:
    

test.pCopula = 
function()
{
    # u - single input value:
    parchmCopula()
    parchmCopula(0.5)
    parchmCopula(0.5, 0.25)
    
    # u - input vector:
    U = (0:10)/10
    V = U
    parchmCopula(U)
    parchmCopula(u = U, v = V)
    parchmCopula(u = U, v = rev(V))
    
    # u - input matrix:
    parchmCopula(cbind(U, V))
    
    # u - input list:
    u = grid2d()
    u
    parchmCopula(u) # output = "vector"
    parchmCopula(u, output = "list")
    diff = parchmCopula(u) - parchmCopula(u, alternative = TRUE)
    mean(abs(diff))
   
    # Check All Types:
    u = grid2d()
    for (type in 1:22) {
        cop1 = parchmCopula(u, type = type, output = "list")
        cop2 = parchmCopula(u, type = type, output = "list", alternative = TRUE)
        cat("Type: ", type, "\t Difference: ", mean(abs(cop1$z-cop2$z)), "\n")
        persp(cop1, main = type, theta = -40, phi = 30, col = "steelblue")
    }
        
    # Try Slider:
    # parchmSlider()
    # parchmSlider("contour")    
  
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------  
# Copula Density:
    

test.dCopula = 
function()
{
    # u - single input value:
    darchmCopula()
    darchmCopula(0.5)
    darchmCopula(0.5, 0.25)
    
    # u - input vector:
    U = (0:10)/10
    V = U
    darchmCopula(U)
    darchmCopula(u = U, v = V)
    darchmCopula(u = U, v = rev(V))
    
    # u - input matrix:
    darchmCopula(cbind(U, V))
    
    # u - input list:
    u = grid2d()
    u
    darchmCopula(u) # output = "vector"
    darchmCopula(u, output = "list")
    
    # Check All Types:
    u = grid2d(x = (0:25)/25)
    for (type in 1:22) {
        cop1 = darchmCopula(u, type = type, output = "list")
        cop2 = darchmCopula(u, type = type, output = "list", 
            alternative = TRUE)
        diff = abs(cop1$z-cop2$z)
        diff = diff[!is.na(diff)]
        cat("Type: ", type, "\t Difference: ", mean(diff), "\n")
        persp(cop2, main = type, theta = -40, phi = 30, col = "steelblue")
    }
        
    # Try Slider:
    # darchmSlider()
    # darchmSlider("contour")
  
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------  
# Parameter Fitting:
    

test.dCopula = 
function()
{

    TYPE = 5
    R = archmCopulaSim(n = 1000, alpha = 1, type = TYPE)
    R[1:10, ]
    plot(R, pch = 19)
    fit = archmCopulaFit(u = R[,1], v = R[,2], type = TYPE)
    fit
    
    # Return Value:
    return()    
} 
        

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCopulae/test/runit3A.R")
    printTextProtocol(testResult)
}
 
  
################################################################################

