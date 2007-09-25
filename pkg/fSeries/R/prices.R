
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
# FUNCTION:                 FINANCIAL TIME SERIES:
#  returns                   Computes returns from a 'timeSeries' object 
#  midquotes                 Computes mid quotes from a 'timeSeries' object
#  spreads                   Computes spreads from a 'timeSeries' object
# OLD FUNCTIONS:
#  returnSeries <- returns
#  getReturns <- returns
#  midquoteSeries <- midquotes
#  spreadSeries <- spreads
################################################################################


returns =
function(x, type = c("continuous", "discrete"), percentage = FALSE, 
trim = TRUE, digits = 8, units = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes returns from a financial price series
    
    # Arguments:    
    #   x - a univariate or multivariate 'timeSeries' object or a  
    #       numeric vector or matrix.
    #   type - a character string specifying how to compute the
    #       returns. Valid choices are: "continuous" and "discrete". 
    #       For the default type = "continuous", the returns are 
    #       calculated as the logarithm differences, otherwise if 
    #       type = "discrete", the returns are calculated as 
    #       percentage changes. 
    #   percentage - by default FALSE, if TRUE the series will be  
    #       expressed in % changes.
    #   trim - a logical flag, by default TRUE, the first missing 
    #       observation in the return series will be removed. 
    #   digits - an integer value, the number of digits of the 
    #       series of returns.
    #   units - a character value or vector which allows to set new 
    #       instrument names to the Data matrix

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:
    
    # Type:
    type = match.arg(type)
    
    # Internal Function for One Column Object:
    getReturnsForOneColumn =
    function(x = x, type = type, percentage = percentage) {
        # Object has to be a vector:
        x = as.vector(x)
        # Continuous: Calculate Log Returns:
        if (type == "continuous") { 
                x = c(NA, diff(log(x))) }   
        # Discrete: Calculate Returns:
        if (type == "discrete") { 
            x = c(NA, diff(c(x, NA))/x)[-(length(x)+1)] }   
        # Percentage Return ?
        if (percentage) { x = x*100 }
        # Return Value:
        x }
        
    # Result:
    if (class(x) == "timeSeries") {
        y = seriesData(x)
        ans = NULL
        for ( i in 1:(dim(y)[[2]]) ) {
            ans = cbind(ans, getReturnsForOneColumn(x = y[,i], 
                type = type, percentage = percentage)) }
        rownames(ans) = rownames(y)
        colnames(ans) = colnames(y)
        ans = new("timeSeries", 
            Data = as.matrix(ans), 
            positions = as.character(x@positions), 
            format = as.character(x@format), 
            FinCenter = as.character(x@FinCenter), 
            units = as.character(x@units), 
            recordIDs = data.frame(),
            title = as.character(x@title), 
            documentation = as.character(x@documentation) ) 
    } else {  
        x = as.vector(x)        
        ans = getReturnsForOneColumn(x = x, type = type, 
            percentage = percentage) 
    }
            
    # Trim:
    if (trim) ans = ans[-1, ]
    # DW: round replaced by signif
    # if (percentage) digits = digits - 2
    ans@Data = signif(ans@Data, digits = digits)
    # DW
    
    # Add New Units:
    if (!is.null(units)){
        ans@units = units
        colnames(ans@Data) = units
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


returnSeries <- returns
getReturns <- returns


# ------------------------------------------------------------------------------


midquotes = 
function(x, which = c("Bid", "Ask"))
{   
    # Compute Mid Quotes:
    midQuotes = 0.5 * ( x[, which[1]] + x[, which[2]] ) 
    
    # Return Value:
    midQuotes
}


# ------------------------------------------------------------------------------


midquoteSeries <- midquotes


# ------------------------------------------------------------------------------


spreads = 
function(x, which = c("Bid", "Ask"), tickSize = NULL)
{   
    # Compute Spread:
    Spread = x[, which[2]] - x[, which[1]] 
    if (!is.null(tickSize)) Spread@Data = round(Spread@Data/tickSize)
    
    # Return Value:
    Spread
}


# ------------------------------------------------------------------------------


spreadSeries <- spreads


################################################################################

