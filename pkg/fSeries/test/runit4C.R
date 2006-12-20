
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
#  .print.garchSpec        S3: Print method for an object of class 'garchSpec'
#  show.garchSpec          S4: Print method for an object of class 'garchSpec'
################################################################################
# PART II - FUNCTION:     SIMULATION:
#  garchSim                Simulates a GARCH/APARCH process
#  .garchSim               Simulates a GARCH/APARCH from specification object
################################################################################
# PART III - FUNCTION:    PARAMETER ESTIMATION: 
#  setClass[fGARCH]        S4: fGARCH Class representation   
#  garchFit                Fits GARCH and APARCH processes
#  .garchFit               ... old Version
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
#  .print.fGARCH           S3 print method for an object of class fGARCH
#  .show.fGARCH            S4 print method for an object of class fGARCH
#  summary.fGARCH          S3 summary method for an object of class fGARCH
#  plot.fGARCH             S3 plot method for an object of class fGARCH
#  .interactiveGarchPlot   Utility Function
#  residuals.fGARCH        S3 residuals method for an object of class fGARCH
#  fitted.fGARCH           S3 fitted values method for an object of class fGARCH
#  predict.fGARCH          S3 prediction method for an object of class fGARCH
# STATISTICS:             Description:
#  .truePersistence        Compute persistence
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


test.garchSpec = 
function()
{   
    # Internal print Function:
    Print = function(x) {
        cat("RUnit Test:\n ")
        print(x@call)
        print(x)
        cat("\n")
    }
    
    # ARCH(1) - use default omega and default alpha[1]
    Print(garchSpec(model = list())) 
    
    # ARCH(1) - use default omega and specify alpha
    Print(garchSpec(model = list(alpha = 0.2))) 
    
    # ARCH(1) - specify omega and alpha
    Print(garchSpec(model = list(omega = 3.0e-6, alpha = 0.3)))
    
    # AR(1)-ARCH(1) - use default omega/alpha and specify alpha[1]
    Print(garchSpec(model = list(ar = 0.5))) 
    
    # AR([1,5])-ARCH(1) - use default omega, specify alpha and subset ar[.]
    Print(garchSpec(model = list(ar = c(0.5,0,0,0,0.1), alpha = 0.25)))
    
    # ARMA(1,2)-ARCH(1) - use default omega/alpha and specify ar[1]/ma[2]
    Print(garchSpec(model = list(ar = 0.5, ma = c(0.3, -0.3))))
    
    # ARMA(2,2)-ARCH(1) use default omega/alpha and specify ar[2]/ma[2]
    Print(garchSpec(model = list(ar = c(0.5, -0.5), ma = c(0.3,-0.3))))
    
    # ARCH(2) - use default omega and specify alpha[2]
    Print(garchSpec(model = list(alpha = c(0.12, 0.04))))
    
    # GARCH(1,1) - use just defaults
    Print(garchSpec())
    
    # GARCH(1,1) - use default omega and specify alpha/beta
    Print(garchSpec(model = list(alpha = 0.2, beta = 0.7)))
    
    # GARCH(1,1) - specify omega/alpha/beta
    Print(garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8)))
    
    # GARCH(1,2) - use default omega and specify alpha[1]/beta[2]
    Print(garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4))))
    
    # GARCH(2,1) - use default omega and specify alpha[2]/beta[1]
    Print(garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08)))
    
    # rsnorm-ARCH(1) - use defaults with skew Normal
    Print(garchSpec(model = list(dist = 2), cond.dist = "rsnorm"))
    
    # rged-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = 4), cond.dist = "rged"))
    
    # rsged-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsged"))
    
    # rstd-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = 4), cond.dist = "rstd"))
    
    # rsstd-ARCH(1) using default omega and alpha[1]
    Print(garchSpec(model = list(dist = c(4, 2)), cond.dist = "rsstd"))
    
    # TS-GARCH(1,1)
    Print(garchSpec(model = list(delta = 1)))
    
    # AR(1)-t-APARCH(2, 1)
    Print(garchSpec(model = list(mu = 1.0e-4, ar = 0.5, omega = 1.0e-6, 
        alpha = c(0.10, 0.05), gamma = c(0, 0), beta = 0.8, delta = 1.8, 
        dist = c(nu = 4, xi = 0.5)), cond.dist = "rsstd"))
        
    # Return Value:
    return() 
}


# ------------------------------------------------------------------------------


test.garchSim = 
function()
{   
    # ARCH(1)
    spec = garchSpec(model = list())
    print(garchSim(n = 10, model = spec))
    
    # ARCH(1)
    spec = garchSpec(model = list(alpha = 0.1))
    print(garchSim(n = 10, model = spec))
    
    # ARCH(1)
    spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1))
    print(garchSim(n = 10, model = spec))
    
    # AR(1)-ARCH(1)
    spec = garchSpec(model = list(ar = 0.5)) 
    print(garchSim(n = 10, model = spec))
    
    # AR([1,5])-ARCH(1)
    spec = garchSpec(model = list(ar = c(0.5,0,0,0,0.1)))
    print(garchSim(n = 10, model = spec))
    
    # ARMA(1,2)-ARCH(1)
    spec = garchSpec(model = list(ar = 0.5, ma = c(0.3,-0.3)))
    print(garchSim(n = 10, model = spec))
    
    # rsnorn-ARCH(2)
    spec = garchSpec(model = list(alpha = c(0.12, 0.04), dist = 2/3), 
        cond.dist = "rsnorm")
    print(garchSim(n = 10, model = spec))
    
    # GARCH(1,1)
    spec = garchSpec()
    print(garchSim(n = 10, model = spec))
    
    # GARCH(1,1)
    spec = garchSpec(model = list(alpha = 0.1, beta = 0.8))
    print(garchSim(n = 10, model = spec))
    
    # GARCH(1,1)
    spec = garchSpec(model = list(omega = 1e-6, alpha = 0.1, beta = 0.8))
    print(garchSim(n = 10, model = spec))
    
    # GARCH(1,2)
    spec = garchSpec(model = list(alpha = 0.1, beta = c(0.4, 0.4)))
    print(garchSim(n = 10, model = spec))
    
    # GARCH(2,1)
    spec = garchSpec(model = list(alpha = c(0.12, 0.04), beta = 0.08))
    print(garchSim(n = 10, model = spec))
    
    # r[s]norm-GARCH(1,1)   
    spec = garchSpec(model = list(), cond.dist = "rnorm")
    print(garchSim(n = 10, model = spec))
    
    spec = garchSpec(model = list(parm = 2), cond.dist = "rsnorm")
    print(garchSim(n = 10, model = spec))
    
    # r[s]ged-GARCH(1,1)
    spec = garchSpec(model = list(parm = 4), cond.dist = "rged")
    print(garchSim(n = 10, model = spec))
    
    spec = garchSpec(model = list(parm = c(4, 2)), cond.dist = "rsged")
    print(garchSim(n = 10, model = spec))
    
    # r[s]std-GARCH(1,1)
    spec = garchSpec(model = list(parm = 4), cond.dist = "rstd")
    print(garchSim(n = 10, model = spec))
    
    spec = garchSpec(model = list(parm = c(4, 2)), cond.dist = "rsstd")
    print(garchSim(n = 10, model = spec))
    
    # TS-GARCH(1,1)
    spec = garchSpec(list(alpha = 0.1, delta = 1))
    print(garchSim(n = 10, model = spec))
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.garchFit = 
function()
{  
    # garchFit(
    #   formula, 
    #   data, 
    #   init.rec = c("mci", "uev"), 
    #   delta = 2, 
    #   skew = 1, 
    #   shape = 4, 
    #   cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
    #   include.mean = TRUE, 
    #   include.delta = NULL, 
    #   include.skew = NULL, 
    #   include.shape = NULL, 
    #   leverage = NULL, 
    #   trace = TRUE, 
    #   algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    #   control = list(), 
    #   title = NULL, 
    #   description = NULL, 
    #   ...)

    # .garchFit(
    #   formula.mean = ~arma(0, 0), 
    #   formula.var = ~garch(1, 1), 
    #   series = x, 
    #   init.rec = c("mci", "uev"), delta = 2, 
    #   skew = 1, 
    #   shape = 4,
    #   cond.dist = c("dnorm", "dsnorm", "dged", "dsged", "dstd", "dsstd"), 
    #   include.mean = TRUE, 
    #   include.delta = NULL, 
    #   include.skew = NULL,
    #   include.shape = NULL, 
    #   leverage = NULL, 
    #   trace = TRUE,  
    #   algorithm = c("sqp", "nlminb", "lbfgsb", "nlminb+nm", "lbfgsb+nm"), 
    #   control = list(), 
    #   title = NULL, 
    #   description = NULL, 
    #   ...)
    
    
    # Use: dem2gbp data frame
    data(dem2gbp)
    print(head(dem2gbp))
    print(class(dem2gbp))
    # As vector:
    x = dem2gbp[,1]
    print(head(x))
    print(class(x))
    
    # GARCH(1,1) - just a vector
    fit = garchFit(~garch(1,1), data = x)
    # GARCH(1,1) - a named data frame
    fit = garchFit(DEM2GBP ~garch(1,1), data = dem2gbp)
    
    # Modify Start Values:
    fit = garchFit(~garch(1,1), x, init.rec = "uev")
    fit@fit$coef
    
    # Skew-Normal Conditional Distribution:
    fit = garchFit(~garch(1,1), x, cond.dist = "dsnorm")
    fit@fit$coef
        
    # Symmetric GED:
    fit = garchFit(~garch(1,1), x, cond.dist = "dged")
    fit@fit$coef
    #          mu       omega      alpha1       beta1       shape 
    # 0.001692849 0.004478847 0.130834725 0.859287121 1.149396980

    # Skew GED - Take the shape from symmetric solution ...
    fit = garchFit(~garch(1,1), x, cond.dist = "dsged", shape = 1.15)     
    fit@fit$coef
    #          mu       omega      alpha1       beta1        skew       shape 
    # -0.02249079  0.01338750  0.22139687  0.73464632  1.18529867  1.13871844 
    # ... don't trust this solution, Hessian fails   

    # Standardized Student-t:
    fit = garchFit(~garch(1,1), x, cond.dist = "dstd")     
    fit@fit$coef
    #          mu       omega      alpha1       beta1       shape 
    # 0.002248651 0.002319034 0.124437902 0.884653277 4.118426687
    # ... misspecified since persistence > 1
    
    # Skew Standardized Student-t:
    # fit = garchFit(~garch(1,1), x, cond.dist = "dsstd")     
    # fit@fit$coef 
    # ... fails                                            
    
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
    
    # garchKappa()
    # garchKappa("dnorm", gamma = 0.3)
    # garchKappa("dged", skew = 0.95, shape = 1)
    
    # garchKappa("dstd", skew = 0.95, shape = 1)
    
    # Return Value:
    return()    
} 
    

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fSeries/test/runit4C.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
