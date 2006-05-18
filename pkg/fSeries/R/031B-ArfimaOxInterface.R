
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
#  *                       Asterisked Functions are in ArmaModelling.R
# FUNCTION:               DESCRIPTION:
#  *fARMA                  Class representation for "fARMA" objects
#  *armaSim                Simulates an ARIMA time series process
#  arfimaOxFit             Fits parameters for AR(FI)MA time series processes
# S3 METHOD:              PREDICTION:
#  *predict.fARMA          S3: Predicts from an ARMA time series prrocess 
#  .arfimaOxPredict            Internal function called by predict.fARMA
#  *predictPlot            S3: Use method
#  *predictPlot.fARMA      S3: Plots from an ARMA time series prediction
# S3 METHOD:              PRINT - SUMMARY - PLOT:
#  *print.fARMA            S3: Prints a fitted ARMA time series object
#  *plot.fARMA             S3: Plots stylized facts of a fitted ARMA object
#  *summary.fARMA          S3: Summarizes a fitted ARMA time series object
#  *fitted.fARMA           S3: Returns fitted values from a fitted ARMA object
#  *residuals.fARMA        S3: Returns residuals from a fitted ARMA object
################################################################################


OXPATH <<- "C:\\Ox\\Ox3"


arfimaOxFit = 
function(formula = x ~ arfima(1, 1), method = c("mle", "nls", "mplik"),
trace = TRUE, title = NULL, description = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits Model Parameters for an ARFIMA Time Series Process
    
    # Argumewnts:
    #   formula - 
    #       x ~ arma(2, 1)
    #       x ~ arima(2, 1, 1)
    #       x ~ arfima(2, 1)
    #   method - 
    #   trace - 
    
    # Notes:
    #   This is an interface to the Arfime Ox Software Package
    
    # Example:
    #   require(fracdiff)
    #   x = as.vector(fracdiff.sim(n=500, ar=0.2, ma=-0.4, d=0.3)$series)
    #   object = arfimaOxFit(x ~ arfima(2, 1))
       
    # FUNCTION:
    
    # Call:
    call = match.call()
    
    # Check for Formula length:
    formula = as.formula(formula)
    if (length(formula) != 3) stop("Formula misspecified")
    
    # Get Series:
    x = eval(formula[[2]], + sys.parent())
    ts = as.vector(x)

    # Which Model - Valid?
    tsmodel =  all.names(formula)[3]
    valid = FALSE
    if (tsmodel == "arma") valid = TRUE
    if (tsmodel == "arima") valid = TRUE
    if (tsmodel == "arfima") valid = TRUE
    if (!valid) stop("Invalid Formula Specification")
    
    # Which Order?
    order = c(0, 0, 0)
    if (tsmodel == "arima") {
        order[1] = as.numeric(as.character(formula[[3]])[2])
        order[2] = as.numeric(as.character(formula[[3]])[3])
        order[3] = as.numeric(as.character(formula[[3]])[4])
    } else {
        order[1] = as.numeric(as.character(formula[[3]])[2])
        order[2] = 0
        order[3] = as.numeric(as.character(formula[[3]])[3])
    }
    
    # Which method ?
    method = 1
    if (method[1] == "nls") method = 2
    if (method[1] == "mplik") method = 3
                                            
    # Write parameters to file - OxArguments.csv:
    parameters = c(
        nt = length(ts), 
        method = method, 
        fixD = 0,
        p = order[1], 
        d = order[2], 
        q = order[3])
    write(x = parameters, file = "OxArguments.csv") 
    
    # Write data to file - OxSeries.csv:
    write(ts, file = "OxSeries.csv", ncolumns = 1)                        
    
    # Estimate Parameters:    
    command = paste(OXPATH, "\\bin\\oxl.exe ", 
        OXPATH, "\\lib\\ArfimaOxFit.ox", sep = "")
    system(command, show.output.on.console = trace, invisible = TRUE)
    
    # Put All Together:
    fit = list()
    fit$call = match.call()
    fit$residuals = scan("OxResiduals.csv", skip = 1, quiet = TRUE)
    fit.parameters = scan("OxParameters.csv", quiet = TRUE)
    nPar = order[1]+1+order[3]
    Names = "d"
    if (order[1] > 0) Names = c(Names, paste("AR-", 1:order[1], sep = "")) 
    if (order[3] > 0) Names = c(Names, paste("MA-", 1:order[3], sep = "")) 
    fit$coef = fit.parameters[1:nPar]
    names(fit$coef) = Names
    fit$se.coef = fit.parameters[(nPar+1):(2*nPar)]
    names(fit$se.coef) = Names
    fit$cov = matrix(fit.parameters[(2*nPar+1):(2*nPar+nPar^2)], ncol = nPar)
    colnames(fit$cov) = rownames(fit$cov) = Names
    fit$sigma2 = c(sigma2 = fit.parameters[2*nPar+nPar^2+1])
    fit$llh = c(llh = fit.parameters[2*nPar+nPar^2+2])
    fit$tstitle = fit$tsmodel = tsmodel
    fit$order = order
    fit$class = "fARMA"
    class(fit) = "list"
       
    # Add title and desription:
    if (is.null(title)) title = "ARFIMA Ox Modelling"
    if (is.null(description)) {
        description = paste(as.character(date()), "by user:", 
            Sys.getenv("USERNAME"))
    }
      
    # Result:
    ans = new("fARMA",     
        call = call,
        formula = as.formula(formula), 
        method = as.character(method),
        parameter = list(include.mean = NA, fixed = NA, order = order),
        data = list(x = x),
        fit = fit,
        residuals = as.vector(fit$residuals),
        fitted.values = as.vector(fit$residuals),
        predicted.values = list(),
        title = as.character(title), 
        description = as.character(description) )
        
    # Return Value:
    ans
} 


# ------------------------------------------------------------------------------


.arfimaOxPredict = 
function(object, n.ahead = 10, n.back = 50, trace = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Predicts from an ARMA Time Series Process
    
    # Note:
    #   This function is called by predict.fARMA()
    
    # FUNCTION:
    
    # Object:
    fit = object@fit
    
    # Write parameters to file - OxArguments.csv:
    x = as.vector(object@data$x)
    nt = length(x)
    ar = fit$order[1]
    d  = fit$order[2]
    ma = fit$order[3]
    write(x = n.ahead, file = "OxArguments.csv") 
    write(x = n.back, file = "OxArguments.csv", append = TRUE) 
    write(x = nt, file = "OxArguments.csv", append = TRUE) 
    write(x = d,  file = "OxArguments.csv", append = TRUE) 
    write(x = ar, file = "OxArguments.csv", append = TRUE) 
    write(x = ma, file = "OxArguments.csv", append = TRUE) 
    for (i in 1:length(fit$coef)) {
        write(x = fit$coef[i], file = "OxArguments.csv", append = TRUE) 
    }

    # Write data to file - OxSeries:
    write(x, file = "OxSeries.csv", ncolumns = 1)   
    
    # Calculate:    
    command = paste(OXPATH, "\\bin\\oxl.exe ", 
        OXPATH, "\\lib\\ArfimaOxPredict.ox", sep = "")
    system(command, show.output.on.console = trace, invisible = TRUE)
    
    # Result:
    mForecast = read.table("OxParameters.csv")
    pred = as.ts(mForecast[, 1])
    se = as.ts(mForecast[, 2])
    ans = list(pred = pred, se = se)

    # Return Value:
    ans
}


################################################################################

