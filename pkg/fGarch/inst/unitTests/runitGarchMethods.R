
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
# METHODS:                DESCRIPTION:
#  show.fGARCH             S4 print method for an object of class 'fGARCH'
#  summary.fGARCH          S3 summary method for an object of class 'fGARCH'
#  plot.fGARCH             S3 plot method for an object of class 'fGARCH'
#  residuals.fGARCH        S3 residuals method for an object of class 'fGARCH'
#  fitted.fGARCH           S3 fitted values for an object of class 'fGARCH'
#  predict.fGARCH          S3 prediction method for an object of class 'fGARCH'
# STATISTICS:             Description:
#  .truePersistence        Compute persistence
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
    
