
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
# FUNCTION:               SPECIFICATION: 
#  'garchSpec'             S4: garchSpec Class representation 
#  garchSpec               S4: Creates a 'garchSpec' object from scratch
#  show.garchSpec          S4: Print method for an object of class 'garchSpec'
# FUNCTION:               SIMULATION:
#  garchSim                Simulates a GARCH/APARCH process
# FUNCTION:               PARAMETER ESTIMATION: 
#  'fGARCH'                S4: fGARCH Class representation   
#  garchFit                Fits GARCH and APARCH processes
# METHODS:                DESCRIPTION:
#  show.fGARCH             S4 print method for an object of class 'fGARCH'
#  summary.fGARCH          S3 summary method for an object of class 'fGARCH'
#  plot.fGARCH             S3 plot method for an object of class 'fGARCH'
#  residuals.fGARCH        S3 residuals method for an object of class 'fGARCH'
#  fitted.fGARCH           S3 fitted values for an object of class 'fGARCH'
#  predict.fGARCH          S3 prediction method for an object of class 'fGARCH'
# STATISTICS:             Description:
#  .truePersistence        Compute persistence
# FUNCTION:               FORECASTING: 
#  garchKappa              Computes Expection for APARCH Models
#  .funE                   Internal function used by kappa()
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(GarchModelling, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


################################################################################


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


################################################################################


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


################################################################################


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

    # *** OLD VERSION ***
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
    
    # Data Frame to Numeric Vector:
    x = dem2gbp[,1]
    print(head(x))
    print(class(x))
    
    # GARCH(1,1) - just a vector:
    fit = garchFit(x ~ garch(1,1), data = x)
    fit = garchFit(~ garch(1,1), data = x)
    
    # GARCH(1,1) - a named data frame:
    fit = garchFit(DEM2GBP ~garch(1,1), data = dem2gbp)
    
    # Modify Start Values:
    fit = garchFit( ~ garch(1,1), x, init.rec = "mci") # default
    fit@fit$coef
    fit = garchFit( ~ garch(1,1), x, init.rec = "uev")
    fit@fit$coef
    
    # Skew Normal Conditional Distribution:
    fit = garchFit(~garch(1,1), x, cond.dist = "dsnorm")
    fit@fit$coef
    #          mu       omega      alpha1       beta1        skew 
    # 0.002418474 0.020628074 0.213000422 0.699638422 0.903313891
        
    # Symmetric GED:
    fit = garchFit(~garch(1,1), x, cond.dist = "dged")
    fit@fit$coef
    #          mu       omega      alpha1       beta1       shape 
    # 0.001692849 0.004478847 0.130834725 0.859287121 1.149396980

    # Skew GED - Take the shape from symmetric solution ...
    fit = garchFit(~garch(1,1), x, cond.dist = "dsged", shape = 1.15)     
    fit@fit$coef
    #          mu       omega      alpha1       beta1        skew       shape 
    # 0.030125742 0.008820863 0.169395870 0.799183865 1.174542338 1.162325254   

    # Standardized Student-t:
    fit = garchFit(~garch(1,1), x, cond.dist = "dstd")     
    fit@fit$coef
    #          mu       omega      alpha1       beta1       shape 
    # 0.002248651 0.002319034 0.124437901 0.884653280 4.118426541
    # Warning - Persistence: 1.009091
    # ... misspecified since persistence > 1
    
    # Skew Standardized Student-t:
    # Default Settings fail, use instead ...    
    fit = garchFit(~garch(1,1), x, cond.dist = "dsstd", algorithm = "nlminb+nm")
    #           mu       omega      alpha1       beta1        skew       shape 
    # -0.008571022 0.002398384 0.124832720 0.883071726 0.913095642 4.201071258 
    # Warning - Persistence: 1.007904                                            
    # ... misspecified since persistence > 1
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.garchFit.initialization = 
function()
{  
    # Load Data:
    data(dem2gbp)  
    # Data Frame to Numeric Vector:
    x = dem2gbp[, 1]
    print(head(x))
    print(class(x))
    
    # Modify Start Values - mci default:
    fit = garchFit( ~ garch(1,1), x, init.rec = "mci") # default
    fit@fit$coef
    
    # Modify Start Values - uev alternative:
    fit = garchFit( ~ garch(1,1), x, init.rec = "uev")
    fit@fit$coef
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------

test.garchFit.distributions = 
function()
{  
    # Load Data:
    data(dem2gbp)  
    # Data Frame to Numeric Vector:
    x = dem2gbp[, 1]
    print(head(x))
    print(class(x))
    
    # Skew Normal Conditional Distribution:
    fit = garchFit(~garch(1,1), x, cond.dist = "dsnorm")
    fit@fit$coef
    #          mu       omega      alpha1       beta1        skew 
    # 0.002418474 0.020628074 0.213000422 0.699638422 0.903313891
        
    # Symmetric GED:
    fit = garchFit(~garch(1,1), x, cond.dist = "dged")
    fit@fit$coef
    #          mu       omega      alpha1       beta1       shape 
    # 0.001692849 0.004478847 0.130834725 0.859287121 1.149396980

    # Skew GED - Take the shape from symmetric solution ...
    fit = garchFit(~garch(1,1), x, cond.dist = "dsged", shape = 1.15)     
    fit@fit$coef
    #          mu       omega      alpha1       beta1        skew       shape 
    # 0.030125742 0.008820863 0.169395870 0.799183865 1.174542338 1.162325254   

    # Standardized Student-t:
    fit = garchFit(~garch(1,1), x, cond.dist = "dstd")     
    fit@fit$coef
    #          mu       omega      alpha1       beta1       shape 
    # 0.002248651 0.002319034 0.124437901 0.884653280 4.118426541
    # Warning - Persistence: 1.009091
    # ... misspecified since persistence > 1
    
    # Skew Standardized Student-t:
    # Default Settings fail, use instead ...    
    fit = garchFit(~garch(1,1), x, cond.dist = "dsstd", algorithm = "nlminb+nm")
    #           mu       omega      alpha1       beta1        skew       shape 
    # -0.008571022 0.002398384 0.124832720 0.883071726 0.913095642 4.201071258 
    # Warning - Persistence: 1.007904                                            
    # ... misspecified since persistence > 1
    
    # Return Value:
    return()    
} 


################################################################################


test.show.fGARCH = 
function()
{ 
    # show.fGARCH - S4 print method for an object of class 'fGARCH'
    
    # Load Data, convert to numeric Vector:
    data(dem2gbp)  
    x = dem2gbp[, 1]
    
    # Fit:
    fit = garchFit(~garch(1,1), data = x, trace = FALSE)
    
    # Print:
    print(fit)
    show(fit)
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.summary.fGARCH = 
function()
{
    # summary.fGARCH - S3 summary method for an object of class 'fGARCH'
    
    # Load Data, convert to numeric Vector:
    data(dem2gbp)  
    x = dem2gbp[, 1]
    
    # Fit:
    fit = garchFit(~garch(1,1), data = x, trace = FALSE)
    
    # Summary:
    summary(fit)
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.plot.fGARCH = 
function()
{
    # plot.fGARCH - S3 plot method for an object of class 'fGARCH'
    
    # Load Data, convert to numeric Vector:
    data(dem2gbp)  
    x = dem2gbp[, 1]
    
    # Fit:
    fit = garchFit(~garch(1,1), data = x, trace = FALSE)
    
    # Plot:
    par(mfrow = c(2, 2), cex = 0.7)
    par(ask = FALSE)
    plot(fit, which = "all")
    
    # Plot - try interactively:
    par(mfrow = c(1, 1))
    plot(fit)
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.residuals.fGARCH = 
function()
{
    # residuals.fGARCH - S3 residuals method for an object of class 'fGARCH'
    
    # Load Data, convert to numeric Vector:
    data(dem2gbp)  
    x = dem2gbp[, 1]
    
    # Fit:
    fit = garchFit(~garch(1,1), data = x, trace = FALSE)
    
    # Residuals:
    residuals(fit)
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.fitted.fGARCH = 
function()
{
    # fitted.fGARCH - S3 fitted values for an object of class 'fGARCH'
    
    # Load Data, convert to numeric Vector:
    data(dem2gbp)  
    x = dem2gbp[, 1]
    
    # Fit:
    fit = garchFit(~garch(1,1), data = x, trace = FALSE)
    
    # Fitted Values:
    fitted(fit)
    
    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


test.predict.fGARCH = 
function()
{
    # predict.fGARCH - S3 prediction method for an object of class 'fGARCH'  

    # Load Data, convert to numeric Vector:
    data(dem2gbp)  
    x = dem2gbp[, 1]
    
    # Fit:
    fit = garchFit(~garch(1,1), data = x, trace = FALSE)
    
    # Predict:
    predict(object = fit, n.ahead = 10, trace = FALSE)
    
    # Return Value:
    return()    
}


################################################################################


test.garchKappa = 
function()
{
    # garchKappa(
    #   cond.dist = c("dnorm", "dged", "dstd", "dsnorm", "dsged", "dsstd"), 
    #   gamma = 0, delta = 2, skew = NA, shape = NA)
    
    garchKappa()
    
    garchKappa("dnorm", gamma = 0.3)
    
    garchKappa("dged", skew = 0.95, shape = 1)
    
    # CHECK !!! garchKappa("dstd", skew = 0.95, shape = 1)
    
    # Return Value:
    return()    
} 
    

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fSeries/tests/runit4C.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################
    
