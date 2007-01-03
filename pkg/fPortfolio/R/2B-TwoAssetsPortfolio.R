
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

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
# FUNCTION:                     DESCRIPTION:
#  frontierTwoAssetsMarkowitz    Computes efficient frontier for Markowitz PF
#  frontierTwoAssetsCVaR         Computes efficient frontier for CVaR PF    
# METHODS:                      DESCRIPTION:
#  print.fPFOLIO2                S3 Print method for a 'fPFOLIO2' object
#  plot.fPFOLIO2                 S3 Plot method for a 'fPFOLIO2' object
#  summary.fPFOLIO2              S3 Summary method for a 'fPFOLIO2' object
################################################################################
 

require(methods)


# ------------------------------------------------------------------------------


setClass("fPFOLIO2", 
    representation(
        call = "call",
        method = "character",
        model = "character",
        data = "data.frame",
        pfolio = "list",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------


frontierTwoAssetsMarkowitz =
function(x, length = 100, title = NULL, description = NULL) 
{   # A Function Implemented by Diethelm Wuertz

    # Description:
    #   Compute the efficient frontier for the Two-Assets
    #   Mean-Variance Markowitz Portfolio. Return the weights
    #   matrix, the portfolios return vector, and the portfolios
    #   risk vector, which is the standard deviation.
    
    # Arguments:
    #   x - a two column matrix of asset returns
    #   weights - a vector of weights, by default ranging from
    #       zero to one in 100 steps
    #   details - a logical flag, should details be printed?
    #       By default TRUE.
    
    # FUNCTION:
    
    # Settings:
    true.length = length - 1
    weights = (0:true.length)/true.length
    
    # Compute Mean-Variance for a two assets portfolio:
    data = as.matrix(x)
    n = dim(data)[1]
    w = weights
    means = apply(data, 2, mean)
    covmat = cov(data)
    Rp <- Vp <- NULL
    for ( i in 1:length(w) ) {
        weights = c(w[i], 1-w[i]) 
        Rp = c(Rp, (weights %*% means)[[1, 1]])
        Vp = c(Vp, (weights %*% covmat %*% weights)[[1, 1]]) 
    }
    SDp = sqrt(Vp)
    
    # Weights: 
    weights = cbind(w, 1-w)
    
    # Result:
    pfolio = list(what = "twoassets", weights = weights, pm = Rp, ps = SDp)
    
    # Title: 
    if (is.null(title)) 
        title = "Two Assets Markowitz Portfolio"
    
    # Description:
    if (is.null(description))
        description = .description()
        
    # Return Value:
    new ("fPFOLIO2", 
        call = as.call(match.call()),
        method = "Exact Analytical Solution", 
        model = "Two Assets Markowitz Portfolio",
        data = as.data.frame(x), 
        pfolio = pfolio, 
        title = as.character(title),
        description = as.character(description)
    )     
}


# ------------------------------------------------------------------------------


frontierTwoAssetsCVaR =
function(x, length = 100, alpha = 0.05, title = NULL, description = NULL)  
{   # A Function Implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the efficient frontier for the Two-Assets
    #   Conditional Value-at-Risk Portfolio. Return the
    #   weights matrix, the portfolios return vector, and 
    #   the portfolios risk vector, which is the CVaR.
    
    # Arguments:
    #
    
    # FUNCTION:
    
    # Settings:
    true.length = length - 1
    weights = (0:true.length)/true.length
    data = as.matrix(x)
    w = weights
    
    # Compute CVaR portfolio for a two assets portfolio:
    n = dim(data)[1]
    means = apply(data, 2, mean)
    Rp <- CVaRp <- NULL
    for (i in 1:length(w) ) {
        weights = c(w[i], 1-w[i])
        Rp = c(Rp, (weights %*% means)[[1, 1]])
        CVaRp = c(CVaRp, -CVaR(data, weights, alpha)) }     
        
    Rp = as.vector(Rp)
    CVaRp = as.vector(CVaRp)
        
    # Weights:
    weights = cbind(w, 1-w)
    
    # Result:
    pfolio = list(what = "twoassets", 
        weightsA = weights[, 1], pm = Rp, ps = CVaRp)
        
    # Title: 
    if (is.null(title)) 
        title = "Two Assets Markowitz Portfolio"
    
    # Description:
    if (is.null(description))
        description = as.character(date())
        
   # Return Value:
    new ("fPFOLIO2", 
        call = as.call(match.call()),
        method = "Exact Analytical Solution", 
        model = "Two Assets CVaR Portfolio",
        data = as.data.frame(x), 
        pfolio = pfolio, 
        title = as.character(title),
        description = as.character(description)
    )     
}


# ------------------------------------------------------------------------------


show.fPFOLIO2 =
function(object) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Print Method for a 'fPFOLIO2' object
    
    # Arguments:
    #   object - an object of class "fPFOLIO"
    
    # FUNCTION:
    
    # Extract Portfolio:
    pfolio = object@pfolio
     
    # Title:
    cat("\nTitle:\n ")
    cat(as.character(object@title), "\n")
    
    # Call:
    cat("\nCall:\n ")
    print.default(object@call)
    
    # Efficient Frontier:
    cat("\nEfficient Frontier - Returns:\n")
    r.digits = abs(round(log10(max(pfolio$pm)), 0)) + 2
    print(round(pfolio$pm, digits = r.digits))
    cat("\nEfficient Frontier - Standard Deviations:\n")
    s.digits = abs(round(log10(max(pfolio$ps)), 0)) + 2
    print(round(pfolio$ps, digits = s.digits))
       
    # Description:
    cat("\nDescription:\n ")
        cat(object@description, "\n")
        
    # Return Value: 
    invisible(object)
}


setMethod("show", "fPFOLIO2", show.fPFOLIO2)


# ------------------------------------------------------------------------------
    

plot.fPFOLIO2 =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Plot Method for a 'fPFOLIO2' object
    
    # Arguments:
    #   x - an object of class 'fPFOLIO2'
    
    # FUNCTION:
    
    # Extract Portfolio:
    pfolio = x@pfolio
    
    # Plot:
    plot(x = pfolio$ps, y = pfolio$pm, 
        xlab = "Risk", ylab = "Return", main = x@title)
        
    # Return Value:
    invisible(x) 
}


# ------------------------------------------------------------------------------


summary.fPFOLIO2 =
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Summary Method for a 'fPFOLIO2' object
    
    # Arguments:
    #   object - an object of class 'fPFOLIO2'
    
    # FUNCTION:
    
    # Print:
    print(object)
    
    # Plot:
    plot(object, ...)  
    
    # Return Value:
    invisible(object) 
}


################################################################################

