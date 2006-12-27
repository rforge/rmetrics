
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
# FUNCTION:          AUGMENTED DICKEY-FULLER TEST:
#  pdftest            Returns probabilities for the ADF Test
#  qdftest            Returns quantiles for the ADF Test
#  dftestTable        Returns augmented Dickey-Fuller finite sample test table
# FUNCTION:          MC KINNON'S PROBABILIY AND QUANTILES:
#  punitroot          Returns cumulative probability for unit root distributions
#  qunitroot          Returns quantiles for unit root distributions
#  unitrootTable      Returns McKinnon's unitroot finite sample test table
# FUNCTION:          INTERNAL UTILITY FUNCTIONS:
#  .strsplitUrcval    Implements string split function for S-Plus compatibility
#  .urcval            Implements unit root statists
#  .probsUrcval       Implements probability values
################################################################################


################################################################################
# AUGMENTED DICKEY-FULLER TEST:


# dftestTable =
adfTable =
function(trend = c("nc", "c", "ct"), statistic = c("t", "n"))
{   # A function implemented by Diethelm Wuertz

    # Description:     
    #   Tables critical values for augmented Dickey-Fuller test.
    
    # FUNCTION:
      
    # Match Arguments:
    type = trend = match.arg(trend)
    statistic = match.arg(statistic)
    
    # Tables:
    if (statistic == "t") {
        # Hamilton Table B.6 - OLS t-Statistic
        if (type == "nc") {
            table = cbind(
                c(-2.66, -2.26, -1.95, -1.60, +0.92, +1.33, +1.70, +2.16),
                c(-2.62, -2.25, -1.95, -1.61, +0.91, +1.31, +1.66, +2.08),
                c(-2.60, -2.24, -1.95, -1.61, +0.90, +1.29, +1.64, +2.03),
                c(-2.58, -2.23, -1.95, -1.62, +0.89, +1.29, +1.63, +2.01),
                c(-2.58, -2.23, -1.95, -1.62, +0.89, +1.28, +1.62, +2.00),
                c(-2.58, -2.23, -1.95, -1.62, +0.89, +1.28, +1.62, +2.00))
        } else if (type == "c") {
            table = cbind(
                c(-3.75, -3.33, -3.00, -2.63, -0.37, +0.00, +0.34, +0.72),
                c(-3.58, -3.22, -2.93, -2.60, -0.40, -0.03, +0.29, +0.66),
                c(-3.51, -3.17, -2.89, -2.58, -0.42, -0.05, +0.26, +0.63),
                c(-3.46, -3.14, -2.88, -2.57, -0.42, -0.06, +0.24, +0.62),
                c(-3.44, -3.13, -2.87, -2.57, -0.43, -0.07, +0.24, +0.61),
                c(-3.43, -3.12, -2.86, -2.57, -0.44, -0.07, +0.23, +0.60))
        } else if (type == "ct") {
            table = cbind(
                c(-4.38, -3.95, -3.60, -3.24, -1.14, -0.80, -0.50, -0.15),
                c(-4.15, -3.80, -3.50, -3.18, -1.19, -0.87, -0.58, -0.24),
                c(-4.04, -3.73, -3.45, -3.15, -1.22, -0.90, -0.62, -0.28),
                c(-3.99, -3.69, -3.43, -3.13, -1.23, -0.92, -0.64, -0.31),
                c(-3.98, -3.68, -3.42, -3.13, -1.24, -0.93, -0.65, -0.32),
                c(-3.96, -3.66, -3.41, -3.12, -1.25, -0.94, -0.66, -0.33))      
        } else {
            stop("Invalid type specified")
        }
    } else if (statistic == "z" || statistic == "n") {
        # Hamilton Table B.5 - Based on OLS Autoregressive Coefficient
        if (type == "nc") {
            table = cbind(
                c(-11.9,  -9.3,  -7.3,  -5.3, +1.01, +1.40, +1.79, +2.28),
                c(-12.9,  -9.9,  -7.7,  -5.5, +0.97, +1.35, +1.70, +2.16),
                c(-13.3, -10.2,  -7.9,  -5.6, +0.95, +1.31, +1.65, +2.09),
                c(-13.6, -10.3,  -8.0,  -5.7, +0.93, +1.28, +1.62, +2.04),
                c(-13.7, -10.4,  -8.0,  -5.7, +0.93, +1.28, +1.61, +2.04),
                c(-13.8, -10.5,  -8.1,  -5.7, +0.93, +1.28, +1.60, +2.03))
        } else if (type == "c") {
            table = cbind(
                c(-17.2, -14.6, -12.5, -10.2, -0.76, +0.01, +0.65, +1.40),
                c(-18.9, -15.7, -13.3, -10.7, -0.81, -0.07, +0.53, +1.22),
                c(-19.8, -16.3, -13.7, -11.0, -0.83, -0.10, +0.47, +1.14),
                c(-20.3, -16.6, -14.0, -11.2, -0.84, -0.12, +0.43, +1.09),
                c(-20.5, -16.8, -14.0, -11.2, -0.84, -0.13, +0.42, +1.06),
                c(-20.7, -16.9, -14.1, -11.3, -0.85, -0.13, +0.41, +1.04))
        } else if (type == "ct") {
            table = cbind(
                c(-22.5, -19.9, -17.9, -15.6, -3.66, -2.51, -1.53, -0.43),
                c(-25.7, -22.4, -19.8, -16.8, -3.71, -2.60, -1.66, -0.65),
                c(-27.4, -23.6, -20.7, -17.5, -3.74, -2.62, -1.73, -0.75),
                c(-28.4, -24.4, -21.3, -18.0, -3.75, -2.64, -1.78, -0.82),
                c(-28.9, -24.8, -21.5, -18.1, -3.76, -2.65, -1.78, -0.84),
                c(-29.5, -25.1, -21.8, -18.3, -3.77, -2.66, -1.79, -0.87))      
        } else {
            stop("Invalid type specified")
        }
    } else {
        stop("Invalid statistic specified")
    }
            
    # Transpose:
    table = t(table)
    colnames(table) = c("0.010", "0.025", "0.050", "0.100", "0.900", 
        "0.950", "0.975", "0.990")
    rownames(table) = c(" 25", " 50", "100", "250", "500", "Inf")
    
    # Add Control:
    attr(table, "control") <-
        c(table = "adf", trend = trend, statistic = statistic)
        
    # Return Value:
    table
} 


# ------------------------------------------------------------------------------


pdftest = 
function(q, n.sample, trend = c("nc", "c", "ct"), statistic = c("t", "n")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Probabilities for the ADF Test
    
    # Arguments:
    
    # FUNCTION:
    
    # Match Arguments:
    trend = match.arg(trend)
    statistic = match.arg(statistic)
    
    # Compute Probabilities:
    X = adfTable(trend = trend, statistic = statistic)
    ans = pTable(t(X), q, n.sample)
    
    # Return Value:
    ans
} 


# ------------------------------------------------------------------------------


qdftest = 
function(p, n.sample, trend = c("nc", "c", "ct"), statistic = c("t", "n"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Quantiles for the ADF Test
    
    # Arguments:
    
    # FUNCTION:
    
    # Match Arguments:
    trend = match.arg(trend)
    statistic = match.arg(statistic)
    
    # Compute Quantiles:
    X = dftestTable(trend = trend, statistic = statistic)
    ans = qTable(X = t(X), p, n.sample)
    
    # Return Value:
    ans
} 


################################################################################
# MC KINNON'S PROBABILIY AND QUANTILES:


punitroot =
function(q, n.sample = 0, trend = c("c", "nc", "ct", "ctt"), 
statistic = c("t", "n"), na.rm = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the cumulative probability for unit root distributions
    
    # Arguments:
    #   q - vector of quantiles 
    #   n.sample - a numerical values giving the number of observations 
    #       from which the test statistics are computed. The default 
    #       value is 0 which means that asymptotic cumulative probabilities  
    #       will be returned.
    #   trend - a character string describing the regression type underlying
    #       the computation of the statistics. Selections are 
    #       "nc" for a constant regression no intercept nor time trend,
    #       "c" for a regression with an intercept but no trend component,
    #       "ct" for a regression including intercept and trend. 
    #       The default value is "c". 
    #   statistic - a character string denoting the type of the test 
    #       statistic. The default choice is "t" for t-statistic, and 
    #       and the alternative choice is "n" for normalized statistic 
    #       also named as rho-statistic.
    #   na.rm - a logical value. If set to TRUE, missing values will 
    #       be removed, the default is FALSE. 
    
    # Value:
    #   A vector of probabilities for the unit root distribution.
    
    # FUNCTION:

    # Settings:
    trend = match.arg(trend)
    statistic = match.arg(statistic)
    
    # Remove any NA's in q:
    if(any(is.na(q))) {
        if(!na.rm) stop("NAs are not allowed when na.rm=FALSE")
        else q = na.omit(q) }

    # Settings - Will be Checked in Routine urcval:
    if (statistic == "t") itt = 1
    if (statistic == "n") itt = 2
    if (trend == "nc")    itv = 1
    if (trend == "c")     itv = 2
    if (trend == "ct")    itv = 3
    if (trend == "ctt")   itv = 4
    
    # Calculate - Call "urcval":
    result = 0 * q
    for (i in 1:length(q)) {
        result[i] = .urcval(arg = q[i], nobs = n.sample, niv = 1, 
            itt = itt, itv = itv, nc = 2) }
            
    # Return Value:
    result  
}


# ------------------------------------------------------------------------------


qunitroot =
function(p, n.sample = 0, trend = c("c", "nc", "ct", "ctt"), 
statistic = c("t", "n"), na.rm = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the quantile for unit root distributions
    
    # Arguments:
    #   q - vector of quantiles 
    #   n.sample - a numerical values giving the number of observations 
    #       from which the test statistics are computed. The default 
    #       value is 0 which means that asymptotic cumulative probabilities  
    #       will be returned.
    #   trend - a character string describing the regression type underlying
    #       the computation of the statistics. Selections are 
    #       "nc" for a constant regression no intercept nor time trend,
    #       "c" for a regression with an intercept but no trend component,
    #       "ct" for a regression including intercept and trend. 
    #       The default value is "c". 
    #   statistic - a character string denoting the type of the test 
    #       statistic. The default choice is "t" for t-statistic, and 
    #       and the alternative choice is "n" for normalized statistic 
    #       also named as rho-statistic.
    #   na.rm - a logical value. If set to TRUE, missing values will 
    #       be removed, the default is FALSE. 

    # Value:
    #   A vector of quantiles for the unit root distribution.
    
    # FUNCTION:
    
    # Settings:
    trend = trend[1]
    statistic = statistic[1]
    
    # Remove any NA's in p:
    if (any(is.na(p))) {
        if(!na.rm) 
            stop("NAs are not allowed when na.rm = FALSE")
        else 
            p = na.omit(p) 
    }

    # Settings - Will be Checked in Routine urcval:
    if (statistic == "t") itt = 1
    if (statistic == "n") itt = 2
    if (trend == "nc")    itv = 1
    if (trend == "c")     itv = 2
    if (trend == "ct")    itv = 3
    if (trend == "ctt")   itv = 4
    
    # Calculate:
    result = 0*p
    for (i in 1:length(p)) {
        result[i] = .urcval(arg = p[i], nobs = n.sample, niv = 1, 
            itt = itt, itv = itv, nc = 1) }
            
    # Return Value:
    result  
}


# ------------------------------------------------------------------------------


unitrootTable =
function(trend = c("c", "nc", "ct", "ctt"), statistic = c("t", "n"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns McKinnon's unitroot finite sample test table
    
    # FUNCTION:
    
    # Match Arguments:
    trend = match.arg(trend)
    statistic = match.arg(statistic)
    
    # Set P Values and Sample Size:
    p = c("0.010", "0.025", "0.050", "0.100", 
          "0.900", "0.950", "0.975", "0.990")
    N = c(" 25", " 50", "100", "250", "500", "Inf")
    
    # Create table:
    table = NULL
    for (n in N[-length(N)]) {
        size = as.integer(n)
        table = rbind(table, qunitroot(as.numeric(p), size, trend, statistic))
    }
    table = rbind(table, qunitroot(as.numeric(p), 0, trend, statistic))
    
    # Round and Add dimnames:
    table = round(table, digits = 3)
    rownames(table) = N
    colnames(table) = p
    
    # Add Control:
    attr(table, "control") <-
        c(table = "unitroot", trend = trend, statistic = statistic)

    # Return Value:
    table 
}


################################################################################
# INTERNAL UTILITY FUNCTIONS:


.strsplitUrcval = 
function(x, sep = " ") 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Implements strsplit() function for SPlus compatibility
    
    # FUNCTION:
    
    # Split:
    if (exists("strsplit")) {
        # R:
        ans = strsplit(x = x , split = sep)
    } else {
        # Splus:
        ans = lapply(lapply(X = x, FUN = unpaste, sep = sep), unlist)
    }
    
    # Return Value:
    ans 
}
    
    
# ------------------------------------------------------------------------------


.urcval = 
function (arg, nobs = 0, niv = 1, itt = 1, itv = 1, nc = 1)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Implements the unit root statists distribution 
    
    # Arguments: 
    #   arg - level of test (between .0001 and .9999) if nc = 1, 
    #       else test statistic if nc = 2
    #   nobs - sample size (0 for asymptotic)
    #   niv - number of integrated variables
    #   itt - 1 or 2 for tau or z test
    #   itv - 1, 2, 3, 4 for nc, c, ct, ctt
    #   nc - 1 or 2 for critical value or P value
    
    # Value:
    #   val - critical value if nc = 1 (returned by routine), 
    #       else P value if nc = 2 (returned by routine)
    
    # FUNCTION:
    
    # Read Probs Table:
    # probs = read.table("library/fSeries/libs/.urcProbs.tab")
    probs = .probsUrcval
    cnorm = probs[, 2]
    probs = probs[, 1]
    
    # Read from appropriate urc Table:
    # fnames = 'urc-1.tab' .... 'urc-12.tab'  (niv) 
    # Skip copyright line and groups of 222 lines as necessary
    skip = 886*(itt-1) + 2*itt + (itv-1)*222
   
    # Filename:
    # root = paste(.tableDir(), "/.urc", sep = "")
    # fname = paste(root, as.character(niv), ".tab", sep = "")
    
    # Get Data:
    TABLE = get(paste(".urc", as.character(niv), sep = ""))
    
    # Read Parameters:
    # parms = scan(fname, what = "", skip = skip - 1, nlines = 1, quiet = TRUE)   
    parms = .strsplitUrcval(TABLE[skip], sep = " ")[[1]]
    parms = parms[parms != ""]
    nz = as.numeric(parms[2])
    nreg = as.numeric(parms[3])
    model = as.numeric(parms[4])
    minsize = as.numeric(parms[5])
    
    # Read Data Group:
    nvar = 4
    if (model == 2 || model == 4) nvar = 3
    nlines = 221 
    # scan(fname, skip = skip, nlines = 221, quiet = TRUE)
    # beta = matrix(SCAN, byrow = TRUE, ncol = nvar+1) 
    beta = matrix(TABLE[(skip+1):(skip+nlines)], ncol = 1)
    beta = unlist(apply(beta, 2, .strsplitUrcval))
    beta = beta[beta != ""]
    beta = matrix(as.numeric(beta), byrow = TRUE, nrow = nlines) 
    wght = t(beta[, nvar+1])
    beta = t(beta[, 1:4])
    
    # Check for Number of Observations:
    if (nobs != 0 && nobs < minsize)
        print("Warning: Specified sample size may be too small")
        
    # Urcvals:
    ans = NULL
    for (i in 1:length(arg)) {
        # Set size and stat and check for level 
        size = stat = arg[i]
        # if (nc == 1) 
        #    stop("Level must be between .0001 and .9999")   
        
        # Calculate: 
        np = 9
        cval = pval = 0.0
        precrt = 2.0
        
        # Call J. G. McKinnon's Fortran Subroutines:
        if (nc == 1) {
            if (min(size) < 0.0001 || max(size) > 0.9999) {
                ans = c(ans, NA) 
            } else {
                result = .Fortran("fcrit", 
                    as.double(probs), as.double(cnorm), as.double(beta), 
                    as.double(wght), as.double(cval), as.double(size), 
                    as.double(precrt), as.integer(nobs), as.integer(model), 
                    as.integer(nreg), as.integer(np), as.integer(0),
                    PACKAGE = "fSeries") 
                ans = c(ans, result[[5]]) 
            } 
        }
        if (nc == 2) {
            result = .Fortran("fpval", 
                as.double(beta), as.double(cnorm), as.double(wght), 
                as.double(probs), as.double(pval), as.double(stat), 
                as.double(precrt), as.integer(nobs), as.integer(model), 
                as.integer(nreg), as.integer(np), as.integer(0),
                PACKAGE = "fSeries") 
            ans = c(ans, result[[5]]) 
        } 
    }
    
    # Return Value:
    ans 
}


# ------------------------------------------------------------------------------


.probsUrcval = 
structure(list(

    V1 = c(1.0e-04, 2.0e-04, 5.0e-04, 0.001, 0.002, 0.003, 
    0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.015, 0.02, 
    0.025, 0.03, 0.035, 0.04, 0.045, 0.05, 0.055, 0.06, 0.065, 0.07, 
    0.075, 0.08, 0.085, 0.09, 0.095, 0.1, 0.105, 0.11, 0.115, 0.12, 
    0.125, 0.13, 0.135, 0.14, 0.145, 0.15, 0.155, 0.16, 0.165, 0.17, 
    0.175, 0.18, 0.185, 0.19, 0.195, 0.2, 0.205, 0.21, 0.215, 0.22, 
    0.225, 0.23, 0.235, 0.24, 0.245, 0.25, 0.255, 0.26, 0.265, 0.27, 
    0.275, 0.28, 0.285, 0.29, 0.295, 0.3, 0.305, 0.31, 0.315, 0.32, 
    0.325, 0.33, 0.335, 0.34, 0.345, 0.35, 0.355, 0.36, 0.365, 0.37, 
    0.375, 0.38, 0.385, 0.39, 0.395, 0.4, 0.405, 0.41, 0.415, 0.42, 
    0.425, 0.43, 0.435, 0.44, 0.445, 0.45, 0.455, 0.46, 0.465, 0.47, 
    0.475, 0.48, 0.485, 0.49, 0.495, 0.5, 0.505, 0.51, 0.515, 0.52, 
    0.525, 0.53, 0.535, 0.54, 0.545, 0.55, 0.555, 0.56, 0.565, 0.57, 
    0.575, 0.58, 0.585, 0.59, 0.595, 0.6, 0.605, 0.61, 0.615, 0.62, 
    0.625, 0.63, 0.635, 0.64, 0.645, 0.65, 0.655, 0.66, 0.665, 0.67, 
    0.675, 0.68, 0.685, 0.69, 0.695, 0.7, 0.705, 0.71, 0.715, 0.72, 
    0.725, 0.73, 0.735, 0.74, 0.745, 0.75, 0.755, 0.76, 0.765, 0.77, 
    0.775, 0.78, 0.785, 0.79, 0.795, 0.8, 0.805, 0.81, 0.815, 0.82, 
    0.825, 0.83, 0.835, 0.84, 0.845, 0.85, 0.855, 0.86, 0.865, 0.87, 
    0.875, 0.88, 0.885, 0.89, 0.895, 0.9, 0.905, 0.91, 0.915, 0.92, 
    0.925, 0.93, 0.935, 0.94, 0.945, 0.95, 0.955, 0.96, 0.965, 0.97, 
    0.975, 0.98, 0.985, 0.99, 0.991, 0.992, 0.993, 0.994, 0.995, 
    0.996, 0.997, 0.998, 0.999, 0.9995, 0.9998, 0.9999), 
    
    V2 = c(-3.71901649, 
    -3.5400838, -3.29052673, -3.09023231, -2.87816174, -2.74778139, 
    -2.65206981, -2.5758293, -2.51214433, -2.45726339, -2.40891555, 
    -2.36561813, -2.32634787, -2.17009038, -2.05374891, -1.95996398, 
    -1.88079361, -1.81191067, -1.75068607, -1.69539771, -1.64485363, 
    -1.59819314, -1.55477359, -1.51410189, -1.47579103, -1.43953147, 
    -1.40507156, -1.37220381, -1.34075503, -1.31057911, -1.28155157, 
    -1.25356544, -1.22652812, -1.20035886, -1.17498679, -1.15034938, 
    -1.12639113, -1.10306256, -1.08031934, -1.05812162, -1.03643339, 
    -1.01522203, -0.99445788, -0.97411388, -0.95416525, -0.93458929, 
    -0.91536509, -0.89647336, -0.8778963, -0.85961736, -0.84162123, 
    -0.82389363, -0.80642125, -0.78919165, -0.77219321, -0.75541503, 
    -0.73884685, -0.72247905, -0.70630256, -0.69030882, -0.67448975, 
    -0.65883769, -0.64334541, -0.62800601, -0.61281299, -0.59776013, 
    -0.58284151, -0.5680515, -0.55338472, -0.53883603, -0.52440051, 
    -0.51007346, -0.49585035, -0.48172685, -0.4676988, -0.45376219, 
    -0.43991317, -0.42614801, -0.41246313, -0.39885507, -0.38532047, 
    -0.37185609, -0.35845879, -0.34512553, -0.33185335, -0.31863936, 
    -0.30548079, -0.2923749, -0.27931903, -0.26631061, -0.2533471, 
    -0.24042603, -0.22754498, -0.21470157, -0.20189348, -0.18911843, 
    -0.17637416, -0.16365849, -0.15096922, -0.13830421, -0.12566135, 
    -0.11303854, -0.10043372, -0.08784484, -0.07526986, -0.06270678, 
    -0.05015358, -0.03760829, -0.02506891, -0.01253347, 0, 0.01253347, 
    0.02506891, 0.03760829, 0.05015358, 0.06270678, 0.07526986, 0.08784484, 
    0.10043372, 0.11303854, 0.12566135, 0.13830421, 0.15096922, 0.16365849, 
    0.17637416, 0.18911843, 0.20189348, 0.21470157, 0.22754498, 0.24042603, 
    0.2533471, 0.26631061, 0.27931903, 0.2923749, 0.30548079, 0.31863936, 
    0.33185335, 0.34512553, 0.35845879, 0.37185609, 0.38532047, 0.39885507, 
    0.41246313, 0.42614801, 0.43991317, 0.45376219, 0.4676988, 0.48172685, 
    0.49585035, 0.51007346, 0.52440051, 0.53883603, 0.55338472, 0.5680515, 
    0.58284151, 0.59776013, 0.61281299, 0.62800601, 0.64334541, 0.65883769, 
    0.67448975, 0.69030882, 0.70630256, 0.72247905, 0.73884685, 0.75541503, 
    0.77219321, 0.78919165, 0.80642125, 0.82389363, 0.84162123, 0.85961736, 
    0.8778963, 0.89647336, 0.91536509, 0.93458929, 0.95416525, 0.97411388, 
    0.99445788, 1.01522203, 1.03643339, 1.05812162, 1.08031934, 1.10306256, 
    1.12639113, 1.15034938, 1.17498679, 1.20035886, 1.22652812, 1.25356544, 
    1.28155157, 1.31057911, 1.34075503, 1.37220381, 1.40507156, 1.43953147, 
    1.47579103, 1.51410189, 1.55477359, 1.59819314, 1.64485363, 1.69539771, 
    1.75068607, 1.81191067, 1.88079361, 1.95996398, 2.05374891, 2.17009038, 
    2.32634787, 2.36561813, 2.40891555, 2.45726339, 2.51214433, 2.5758293, 
    2.65206981, 2.74778139, 2.87816174, 3.09023231, 3.29052673, 3.5400838, 
    3.71901649)), 
    
    .Names = c("V1", "V2"), 
    
    class = "data.frame", 
    
    row.names = c(
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", 
    "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
    "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", 
    "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", 
    "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", 
    "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", 
    "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", 
    "91", "92", "93", "94", "95", "96", "97", "98", "99", "100", 
    "101", "102", "103", "104", "105", "106", "107", "108", "109", 
    "110", "111", "112", "113", "114", "115", "116", "117", "118", 
    "119", "120", "121", "122", "123", "124", "125", "126", "127", 
    "128", "129", "130", "131", "132", "133", "134", "135", "136", 
    "137", "138", "139", "140", "141", "142", "143", "144", "145", 
    "146", "147", "148", "149", "150", "151", "152", "153", "154", 
    "155", "156", "157", "158", "159", "160", "161", "162", "163", 
    "164", "165", "166", "167", "168", "169", "170", "171", "172", 
    "173", "174", "175", "176", "177", "178", "179", "180", "181", 
    "182", "183", "184", "185", "186", "187", "188", "189", "190", 
    "191", "192", "193", "194", "195", "196", "197", "198", "199", 
    "200", "201", "202", "203", "204", "205", "206", "207", "208", 
    "209", "210", "211", "212", "213", "214", "215", "216", "217", 
    "218", "219", "220", "221")
)


################################################################################

