
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
# FUNCTION:             NORMALITY TESTS:
#  'fHTEST'              S4 Class Representation
#  show.fHTEST           S4 Print Method
# FUNCTION:             DESCRIPTION:
#  jbTable               Finite sample p values for the Jarque Bera test
# FUNCTION:             PVALUE AND STATISTICS TABLES:
#  pPlot                 Displays a general finite sample probability plot
#  pTable                Interpolates probabilities from a finite sample table
#  .pTable                Utility function called by the function 'pTable'
#  qTable                Interpolates quantiles from a finite sample table
#  .qTable                Utility function called by the function 'qTable'
# FUNCTION:             INTERNAL FUNCTIONS:
#  .interpTable.old       Akima spline interpolation utility function
#  .interpTable.new       Akima spline interpolation utility function
################################################################################


setClass("fHTEST", 
    representation(
        call = "call",
        data = "list",
        test = "list",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------


show.fHTEST = 
function(object)
{   # A function implemented by Diethelm Wuertz

    # Source:
    #   This function copies code from base:print.htest
 
    # FUNCTION:
       
    # Unlike print the argument for show is 'object'.
    x = object
    
    # Title:
    cat("\nTitle:\n ", x@title, "\n", sep = "")
    
    # Call:
    # cat("\nCall:\n", deparse(x@call), "\n", sep = "")
    
    # Data Name:
    # cat("\nData Name:\n", ans@data.name, "\n", sep = "")  
    
    # Test Results:
    test = x@test
    cat("\nTest Results:\n", sep = "")
    
    # Tests from tseries package:
    
    # Parameter:
    if (!is.null(test$parameter)) {
        parameter = test$parameter
        Names = names(parameter)
        cat("  PARAMETER:\n")
        for ( i in 1: length(Names) )
            cat(paste("    ", names(parameter[i]), ": ", 
                format(round(parameter[i], 3)), "\n", sep = "") )
    }
    
    # Sample Estimates:
    if (!is.null(test$estimate)) {
        estimate = test$estimate
        Names = names(estimate)
        cat("  SAMPLE ESTIMATES:\n")
        for (i in 1:length(Names)) {        
            cat(paste("    ", Names[i], ": ", 
                round(estimate[i], digits = 4), "\n", sep = "" ) )  
        }   
    }
    
    # Statistic:
    if (!is.null(test$statistic)) {
        statistic = test$statistic
        Names = names(statistic)
        cat("  STATISTIC:\n")
        for (i in 1:length(Names)) {        
            if (!is.na(statistic[i])) {
                cat(paste("    ", Names[i], ": ",
                    round(statistic[i], digits = 4), "\n", sep = "" ) )  
            }
        }
    } 
             
    # P-Value:
    if (!is.null(test$p.value)) {
        pval = test$p.value
        Names = names(pval)
        if (Names[1] == "") space = "" else space = ": "
        cat("  P VALUE:\n")     
        for (i in 1:length(Names)) {
            if (!is.na(pval[i])) {
                if (class(version) != "Sversion") {
                    cat(paste("    ", Names[i], space, 
                    format.pval(pval[i], digits = 4), " \n", sep = "" ) ) 
                } else {
                    cat(paste("    ", Names[i], space, 
                    round(pval[i], digits = 4), " \n", sep = "" ) )  
                }
            }
        }
    }
         
    # Confidence Interval:
    if (!is.null(test$conf.int)) {
        conf = test$conf.int
        # For SPlus compatibility use dimnames istead of colnames!
        colNames = dimnames(conf)[[2]]
        cat("  CONFIDENCE INTERVAL:\n")
        for (i in 1:length(colNames)) {     
            cat(paste("    ", colNames[i], ": ",
                round(conf[1, i], digits = 4), ", ", 
                round(conf[2, i], digits = 4), "\n", sep = "" ) )  
        }   
    }
    
    # More Specific Output Results:
    if (!is.null(test$output)) {
        cat(test$output, fill = FALSE, sep = "\n")
    }
    
    # Description:
    cat("\nDescription:\n ", x@description, sep = "")   
    cat("\n\n")
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


setMethod("show", "fHTEST", show.fHTEST)


################################################################################
#  jbTable               Finite sample p values for the Jarque Bera test


jbTable = 
function(type = c("LM", "ALM"), size = c("all", "small"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Finite sample p values for the Jarque Bera test
    
    # Details:
    #   The function jbTable() returns a data.frame with columns denoting 
    #   size and rows denoting probabilities 0 < p < 1.
   
    # FUNCTION:
    
    # Create Table:
    if (type[1] == "LM") {
        table = .jbLM
    } else if (type[1] == "ALM") { 
        table = .jbALM
    }
    
    # Downsize Data:
    if (size[1] == "small") {
        n = dim(table)[1]
        table = table[c(matrix(1:(n-2), byrow = TRUE, ncol = 22)[, 1], n), ]
        table = table[-(1:17),]
    }

    # Return Value:
    table
}


################################################################################
#  pPlot                 General finite sample probability plot
#  pTable                Interpolated probabilities from finite sample table
#  .pTable                Utility function called by the function 'pTable'
#  qTable                Interpolated quantiles from finite sample table
#  .qTable                Utility function called by the function 'qTable'


pPlot = 
function(X, nN = 100, nStat = 100, logN = TRUE, logStat = FALSE, 
fill = FALSE, linear = TRUE, digits = 8, doplot = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   General finite sample probability plot
    
    # Details:
    #   This function creates from regularly spaced data 'Statistic(p,N)' 
    #   a new regularly spaced data array 'p(Statistic,N)' using Akima's
    #   spline interpolation. 
    #   Optionally a perspective plot will be created.
    
    # Arguments:
    #   X - a data frame or table with q values for given sizes 'N'
    #       and probabilities 'p'. The colnames must contain the
    #       size values and row names must contain the p values.
    #   nN - the number of points to be interpolated for the 'size'
    #   nStat - the number of points to be interpolated for the 'Statistic'
    #   logN - should a logical scale be used for the 'size' values?
    #   logStat - should a logical scale be used for the 'Statistic' values?
    #   fill - should be NA's filled with asymptotic values zero and one? 
    #       By default no.
    #   digits - the number of digits used for the output. By default 8
    #   doplot - should a plot be created? By default yes.
    
    # Note:
    #   For large arrays 'X' this function will take some time to run. 
    #   Please, be patient.
    
    # Examples:
    #   pPlot(X = adfTable(), main = "const ADF")
    #   pPlot(X = adfTable(), fill = TRUE, main = "const ADF")
      
    # FUNCTION:
    
    # Akima Interpolation:
    # require(akima)
    X = as.matrix(X)
    
    # Settings:
    N = as.integer(colnames(X))
    p = as.numeric(rownames(X))
    N[1] = 1.0001*N[1]
    N[length(N)] = 0.9999*N[length(N)]
    
    
    # Create from Matrix X Vector Data:
    x.vec = rep(N, times = length(p))         # N Values
    y.vec = as.vector(t(X))                   # Statistic
    z.vec = rep(p, each = length(N))          # p Value
    
    # Generate Sampling Points - N/Statistic Values:
    if (logN) {
        xo = log10(10^seq(log10(1.01*min(x.vec)), log10(0.99*max(x.vec)), 
            length = nN))
        x.vec = log10(x.vec)
    } else {
        xo = seq(1.01*min(x.vec), 0.99*max(x.vec), length = nN)  
    }
    xo = round(xo, digits = digits)     
    if (logStat) {
        yo = log10(10^seq(log10(1.01*min(y.vec)), log10(0.99*max(y.vec)), 
            length = nStat))
        y.vec = log10(y.vec)
    } else {
        yo = seq(1.01*min(y.vec), 0.99*max(y.vec), length = nStat)  
    }
    yo = round(yo, digits = digits) 
    
    # Interpolation on Grid:
    # 'interp.new' ignores ncp and does only bicubic spline interpolation.
    if (linear) {
       ans = .interpTable.old(x = x.vec, y = y.vec, z = z.vec, xo = xo, 
            yo = yo, ncp = 0, extrap = FALSE, duplicate = "median") 
    } else {
    # There is a bug in interp.new ...
        ans = .interpTable.new(x = x.vec, y = y.vec, z = z.vec, xo = xo, 
            yo = yo, linear = FALSE, extrap = FALSE, duplicate = "median")
    }
    zo = as.matrix(ans$z)
    
    # Fill and Correct Asymptotic Values:
    if (fill) {
        m1 = trunc(0.50*nStat)
        for (i in 1:nN) {
            for (j in  1:m1)    if (is.na(zo[i, j])) zo[i, j] = 0
            for (j in m1:nStat) if (is.na(zo[i, j])) zo[i, j] = 1
            # Cut values larger than 1 and smaller than 0:
            for (j in  1:nStat) if (zo[i, j] > 1) zo[i, j] = 1
            for (j in  1:nStat) if (zo[i, j] < 0) zo[i, j] = 0
        }
    }
    
    # Add Column and Row Names:
    rownames(zo) = as.character(xo)
    colnames(zo) = as.character(yo)
    ans = list(x = xo, y = yo, z = round(zo, digits))
    attr(ans, "params") = unlist(list( nN = nN, nStat = nStat, 
        logN = logN, logStat = logStat, fill = fill, digits = digits ))
              
    # Plot:
    if (doplot) {
        if (logN) logN.lab = "log10" else logN.lab = ""
        if (logStat) logStat.lab = "log10" else logStat.lab = ""
        persp(ans, theta = 40, phi = 30, xlab = paste(logN.lab, "N"), 
            ylab = paste(logStat.lab, "Statistic"), zlab = "p Value", 
            ticktype = "detailed", col = "grey", shade = 0.5, ...)
    }

    # Return Value:
    invisible(ans)
}

    
# ------------------------------------------------------------------------------
    
    
qTable =
function(X, p, N, digits = 4)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Interpolated quantiles from finite sample table.
    
    # Arguments:
    #   X - a data frame or matrix with q values for given sizes 'N'
    #       and probabilities 'p'. The colnames must contain the
    #       size values and row names must contain the p values.
    #   p - a vector or numeric p value to be interpolated.
    #   N - an integer size value to be interpolated.
    #   digits - the number of digits used for the output.
    
    # Value:
    #   Interpolated quantiles for size N and p values p.
    
    # Examples:
    #   plot(qTable(X = adfcTable(), p = (1:99)/100, N = 100), type = "l")
    #   qTable(X = adfcTable(), p = 0.075, N = 175,  digits = 2)
    #   qTable(jblmTable(), N = 100, p = (1:99)/100, lower.tail = FALSE)
 
    # FUNCTION:
    
    # Check:
    if (length(N) != 1) stop("N must be of length 1") 
    
    # Iterate:
    q = NULL
    for ( P in p )
        q = c(q, .qTable(X, P, N, digits))
        
    # Return Value
    q
}


# ------------------------------------------------------------------------------


.qTable = 
function(X, p, N, digits = 4)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Utility function called by the function 'qTable'
    
    # Arguments:
    #   X - a data frame or matrix with q values for given sizes 'N'
    #       and probabilities 'p'. The colnames must contain the
    #       size values and row names must contain the p values.
    #   p - a numeric p value to be interpolated.
    #   N - an integer size value to be interpolated.
    #   digits - the number of digits used for the output.
     
    # FUNCTION:
        
    # Positions:
    z = as.matrix(X)
    x = Ps = as.numeric(rownames(X))
    if (p > max(Ps)) return(NA)
    if (p < min(Ps)) return(NA) 
    y = Ns = as.numeric(colnames(X))
    if (N > max(Ns)) return(NA)
    if (N < min(Ns)) return(NA)
    w = which(Ps == p)
    if ( length(w) == 0 ) {
        xo = p
        yo = N
        nx = length(x[x < xo])
        ny = length(y[y < yo])
        p = (xo - x[nx] ) / ( x[nx+1] - x[nx] )
        q = (yo - y[ny] ) / ( y[ny+1] - y[ny] )    
        # Value:
        zo = (1-p)*(1-q)*z[nx, ny] + p*(1-q)*z[nx+1, ny] + 
            q*(1-p)*z[nx, ny+1] + p*q*z[nx+1,ny+1] 
        if (length(zo) == 0) zo = NA
    } else {
        # Value:
        zo = approx(x = Ns, y = z[w, ], xout = N)$y
    }

    # Return Value:
    round(zo, digits)
}


# ------------------------------------------------------------------------------
    

pTable = 
function(X, Stat, N, digits = 4) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Interpolated probabilities from finite sample table
    
    # Arguments:
    #   X - a data frame or matrix with q values for given sizes 'N'
    #       and statistic 'Stat'. The column names must contain the
    #       size values and the row names must contain the p values.
    #   Stat - a vector or numeric quantile value to be interpolated.
    #   N - an integer size value to be interpolated.
    #   digits - the number of digits used for the output.
    
    # Examples:
    #   pTable(X = cADF, N = 100, Stat = -2.89)    
    #   pTable(X = jbLM, N = 100, Stat = 5.43) 
    #   pTable(X = jbLM, N = 1400, Stat = 0.7003) 
  
    # FUNCTION:
    
    # Check:
    if (length(N) != 1) stop("N must be of length 1") 
    
    # Iterate:
    p = NULL
    for ( STAT in Stat )
        p = c(p, .pTable(X, STAT, N, digits))
        
    # Return Value
    p
}


# ------------------------------------------------------------------------------


.pTable = 
function(X, Stat, N, digits = 4) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Utility function called by the function 'pTable'
    
    # Arguments:
    #   X - a data frame or matrix with q values for given sizes 'N'
    #       and statistic 'Stat'. The column names must contain the
    #       size values and the row names must contain the p values.
    #   Stat - a numeric quantile value to be interpolated.
    #   N - an integer size value to be interpolated.
    #   digits - the number of digits used for the output.
    
    # Value:
    #   Interpolated probabilities for size 'N' and quantiles 'Stat'.

    # FUNCTION:
    
    # Extract a proper part from the table to speed up the execution time:
    table = t(as.matrix(X))
    tablep = as.numeric(colnames(table))
    tableT = as.numeric(rownames(table))
    tablen = dim(table)[2]
    tableipl = numeric(tablen) 
    
    # p's - by Column:
    if (N < min(tableT)) {
         warning(paste("N must be greater than", min(tableT)))
         PVAL = NA
         return(PVAL)
    }
    if (N > max(tableT)) {
         warning(paste("N must be smaller than", max(tableT)))
         PVAL = NA
         return(PVAL)
    }
         
    # Interpolate Data:
    for (i in (1:tablen)) 
        tableipl[i] = approx(tableT, table[, i], N, rule = 2)$y
    PVAL = approx(tableipl, tablep, Stat, rule = 2)$y 
    
    # Ckeck Consistency:  
    if (is.na(approx(tableipl, tablep, Stat, rule = 1)$y)) {
        if (PVAL == min(tablep)) {
            warning("p-value smaller/greater than printed p-value")
        }
        else {
            warning("p-value greater/smaller than printed p-value")
        }
    }
   
    # Return Value:
    round(PVAL, digits = 4)
}


################################################################################
#  .interpTable.old       Akima spline interpolation utility function
#  .interpTable.new       Akima spline interpolation utility function


.interpTable.old = 
function (x, y, z, xo = seq(min(x), max(x), length = 40), yo = seq(min(y), 
max(y), length = 40), ncp = 0, extrap = FALSE, duplicate = "error", 
dupfun = NULL) 
{   #  A copy from contributed package akima

    # FUNCTION:
    
    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z)))) 
        stop("missing values and Infs not allowed")
    drx <- diff(range(x))
    dry <- diff(range(y))
    if (drx == 0 || dry == 0) 
        stop("all data collinear")
    if (drx/dry > 10000 || drx/dry < 1e-04) 
        stop("scales of x and y are too dissimilar")
    n <- length(x)
    nx <- length(xo)
    ny <- length(yo)
    if (length(y) != n || length(z) != n) 
        stop("Lengths of x, y, and z do not match")
    xy <- paste(x, y, sep = ",")
    i <- match(xy, xy)
    if (duplicate == "user" && !is.function(dupfun)) 
        stop("duplicate=\"user\" requires dupfun to be set to a function")
    if (duplicate != "error") {
        centre <- function(x) {
            switch(duplicate, mean = mean(x), median = median(x), 
                user = dupfun(x))
        }
        if (duplicate != "strip") {
            z <- unlist(lapply(split(z, i), centre))
            ord <- !duplicated(xy)
            x <- x[ord]
            y <- y[ord]
            n <- length(x)
        } else {
            ord <- (hist(i, plot = FALSE, freq = TRUE, breaks = 
                seq(0.5, max(i) + 0.5, 1))$counts == 1)
            x <- x[ord]
            y <- y[ord]
            z <- z[ord]
            n <- length(x)
        }
    } else if (any(duplicated(xy))) 
        stop("duplicate data points")
    zo <- matrix(0, nx, ny)
    storage.mode(zo) <- "double"
    miss <- !extrap
    misso <- matrix(miss, nx, ny)
    if (extrap & ncp == 0) 
        warning("Cannot extrapolate with linear option")
    ans <- .Fortran("idsfft", as.integer(1), as.integer(ncp), 
        as.integer(n), as.double(x), as.double(y), as.double(z), 
        as.integer(nx), as.integer(ny), x = as.double(xo), y = as.double(yo), 
        z = zo, integer((31 + ncp) * n + nx * ny), double(5 * 
            n), misso = as.logical(misso), PACKAGE = "fBasics")
    temp <- ans[c("x", "y", "z", "misso")]
    temp$z[temp$misso] <- NA
    temp[c("x", "y", "z")]
}


# ------------------------------------------------------------------------------


.interpTable.new = 
function (x, y, z, xo = seq(min(x), max(x), length = 40), yo = seq(min(y), 
    max(y), length = 40), linear = FALSE, ncp = NULL, extrap = FALSE, 
    duplicate = "error", dupfun = NULL) 
{   #  A copy from contributed package akima

    # FUNCTION:
    
    if (!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z)))) 
        stop("missing values and Infs not allowed")
    if (!is.null(ncp)) {
        if (ncp != 0) {
            cat("ncp not supported, it is automatically choosen by Fortran code\n")
        } else {
            cat("linear interpolation not yet implemented with interp.new().\n")
            stop("use interp.old().")
        }
    }
    if (linear) {
        cat("linear interpolation not yet implemented with interp.new().\n")
        stop("use interp.old().")
    }
    drx <- diff(range(x))
    dry <- diff(range(y))
    if (drx == 0 || dry == 0) 
        stop("all data collinear")
    if (drx/dry > 10000 || drx/dry < 1e-04) 
        stop("scales of x and y are too dissimilar")
    n <- length(x)
    nx <- length(xo)
    ny <- length(yo)
    if (length(y) != n || length(z) != n) 
        stop("Lengths of x, y, and z do not match")
    xy <- paste(x, y, sep = ",")
    i <- match(xy, xy)
    if (duplicate == "user" && !is.function(dupfun)) 
        stop("duplicate=\"user\" requires dupfun to be set to a function")
    if (duplicate != "error") {
        centre <- function(x) {
            switch(duplicate, mean = mean(x), median = median(x), 
                user = dupfun(x))
        }
        if (duplicate != "strip") {
            z <- unlist(lapply(split(z, i), centre))
            ord <- !duplicated(xy)
            x <- x[ord]
            y <- y[ord]
            n <- length(x)
        } else {
            ord <- (hist(i, plot = FALSE, freq = TRUE, breaks = seq(0.5, 
                max(i) + 0.5, 1))$counts == 1)
            x <- x[ord]
            y <- y[ord]
            z <- z[ord]
            n <- length(x)
        }
    } else if (any(duplicated(xy))) 
        stop("duplicate data points")
    zo <- matrix(0, nx, ny)
    storage.mode(zo) <- "double"
    miss <- !extrap
    extrap <- matrix(TRUE, nx, ny)
    if (!is.null(ncp)) {
        # DW
        if (prod(extrap) & ncp == 0) 
            warning("Cannot extrapolate with linear option")
    } else {
        # DW
        if (prod(extrap) & linear) 
            warning("Cannot extrapolate with linear option")
    }
    ans <- .Fortran("sdsf3p", as.integer(1), as.integer(n), as.double(x), 
        as.double(y), as.double(z), as.integer(nx), x = as.double(xo), 
        as.integer(ny), y = as.double(yo), z = zo, ier = integer(1), 
        double(36 * n), integer(25 * n), extrap = as.logical(extrap), 
        near = integer(n), nxt = integer(n), dist = double(n), 
        PACKAGE = "fBasics")
    temp <- ans[c("x", "y", "z", "extrap")]
    if (miss) 
        temp$z[temp$extrap] <- NA
    temp[c("x", "y", "z")]
}


################################################################################

