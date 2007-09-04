
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
# FUNCTION:               PARAMETER ESTIMATION: 
#  'fGARCH'                S4: fGARCH Class representation   
#  garchFit                Fits GARCH and APARCH processes
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
    
