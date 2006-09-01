
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
# PART I - FUNCTION:      SPECIFICATION: 
#  setClass[garchSpec]     S4: garchSpec Class representation 
#  garchSpec               S4: Creates a 'garchSpec' object from scratch
#  print.garchSpec         S3: Print method for an object of class 'garchSpec'
#  .garchSpecRUnit         RUnit Testing
################################################################################
# PART II - FUNCTION:     SIMULATION:
#  garchSim                Simulates a GARCH/APARCH process
#  .garchSim               Simulates a GARCH/APARCH from specification object
#  .garchSimRUnit          RUnit Testing
################################################################################
# PART III - FUNCTION:    PARAMETER ESTIMATION: 
#  setClass[fGARCH]        S4: fGARCH Class representation   
#  garchFit                Fits GARCH and APARCH processes
#  .garchInitSeries        Initializes Series
#  .garchInitParameters    Initializes Parameters
#  .garchSetCondDist       Selects conditional density function
#   .garchDist              Defines conditional density function
#  .garchOptimizeLLH       Opimizes log-likelihood function by 'nlminb'/'bfgs'
#   .garchLLH               Computes log-likelihood function
#   .garchHessian           Computes Hessian matrix numerically
#  .garchNames              Slot names, @fit slot, parameters and controls
#  .garchTsFit             Wrapper for 'garch()' from 'tseries' package
# METHODS:                DESCRIPTION:
#  print.fGARCH            S3 Print method for an object of class fGARCH
#  summary.fGARCH          S3 Summary method for an object of class fGARCH
#  plot.fGARCH             S3 Plot method for an object of class fGARCH
#  .interactiveGarchPlot   Utility Function
#  residuals.fGARCH        S3 Residuals method for an object of class fGARCH
#  fitted.fGARCH           S3 Fitted values method for an object of class fGARCH
#  predict.fGARCH          S3 Prediction method for an object of class fGARCH
# STATISTICS:             Description:
#  .truePersistence        Compute Persistence
################################################################################
# PART IV - FUNCTION:     FORECASTING: 
#  garchKappa               Computes Expection for APARCH Models
#  .funE                    Internal function used by kappa()
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(GarchModelling); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.garchSim = 
function()
{  
    # garchSpec(model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8), 
    #     presample = NULL, cond.dist = c("rnorm", "rged", "rstd",  
    #    "rsnorm", "rsged", "rsstd"), rseed = NULL)

    # garchSim(model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8),  
    #    n = 100, n.start = 100, presample = NULL, cond.dist = c("rnorm", 
    #    "rged", "rstd", "rsnorm", "rsged", "rsstd"), rseed = NULL)
    
    # Simulation:
    
    model = list(omega = 1.0e-6, alpha = 0.1, beta = 0.8)
    spec = garchSpec(model = model)
    spec
    slotNames(spec)
    
    garchSim(spec)
    sim = garchSim(model)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.garchFit = 
function()
{  
    # garchFit(formula.mean = ~arma(0, 0), formula.var = ~garch(1, 1), 
    #    series = x, init.rec = c("mci", "uev"), delta = 2, skew = 1, shape = 4,
    #    cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
    #    include.mean = TRUE, include.delta = NULL, include.skew = NULL,
    #    include.shape = NULL, leverage = NULL, trace = TRUE,  
    #    algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    #    control = list(), title = NULL, description = NULL, ...)
    
    
    data(dem2gbp)
    x = as.vector(dem2gbp[,1])
    
    # GARCH(1,1)
    G = garchFit()
    G
    residuals(G)
    fitted(G)
    
    garchFit(init.rec = "uev")
    garchFit(cond.dist = "dsnorm")
    garchFit(cond.dist = "dged")
    garchFit(cond.dist = "dsged")               # Fails
    garchFit(cond.dist = "dstd")
    garchFit(cond.dist = "dsstd")               # Fails
    garchFit(include.mean = FALSE)
    garchFit(delta = 1, include.delta = FALSE)
    garchFit(include.delta = TRUE)              # FAILS
    
    garchFit(formula.var = ~aparch(1,1))
    
    # Return Value:
    return()    
}   


# ------------------------------------------------------------------------------


test.garchKappa = 
function()
{
    # garchKappa(
    #   cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
    #   gamma = 0, delta = 2, skew = NA, shape = NA)
    
    garchKappa()
    garchKappa("dnorm", gamma = 0.3)
    garchKappa("dged", skew = 0.95, shape = 1)
    
    garchKappa("dstd", skew = 0.95, shape = 1)
    
    # Return Value:
    return()    
} 
    

# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fSeries/test/runit034C.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
