
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
# FUNCTION:          DICKEY FULLER TEST:
#  pdftest            Returns probabilities for the ADF Test
#  qdftest            Returns quantiles for the ADF Test
#  .dfTable           Augmented Dickey-Fuller finite sample test table
# FUNCTION:          PROBABILIY AND QUANTILES:
#  punitroot          Returns cumulative probability for unit root distributions
#  qunitroot          Returns quantiles for unit root distributions
# INTERNAL:          UTILITY FUNCTIONS:
#  .strsplitUrcval    String split function for S-Plus compatibility
#  .urcval            Implements unit root statists
#  .probsUrcval       Probability values
#  .urc1 ... .urc12   Statistical values
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(UnitrootDistribution); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.dfTable = 
function()
{ 
    # Dickey-Fuller Test Tables:
    #   .dfTable(trend = c("nc", "c", "ct"), statistic = c("t", "n"))
    
    Table = .dfTable("nc", "t")
    print(Table)
    checkSum = -15.11
    checkEqualsNumeric(target = sum(Table), current = checkSum)
    
    Table = .dfTable("c", "t")
    print(Table)
    sum(Table) 
    checkSum = -70.55
    checkEqualsNumeric(target = sum(Table), current = checkSum)
    
    Table = .dfTable("ct", "t")
    print(Table)
    sum(Table)
    checkSum = -104.68
    checkEqualsNumeric(target = sum(Table), current = checkSum)  
    
    Table = .dfTable("nc", "n")
    print(Table)
    sum(Table)
    checkSum = -184.07
    checkEqualsNumeric(target = sum(Table), current = checkSum) 
    
    Table = .dfTable("c", "n")
    print(Table)
    sum(Table)
    checkSum = -357.11 
    checkEqualsNumeric(target = sum(Table), current = checkSum)
    
    Table = .dfTable("ct", "n")
    print(Table)
    sum(Table)
    checkSum = -582.60
    checkEqualsNumeric(target = sum(Table), current = checkSum)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.dfQuantiles = 
function()
{    
    # pdftest(q, n.sample, trend = c("nc", "c", "ct"), statistic = c("t", "n")) 
    # qdftest(p, n.sample, trend = c("nc", "c", "ct"), statistic = c("t", "n"))
    
    p = 0.984
    n.sample = 78
    for (trend in c("nc", "c", "ct")) {
        for (statistic in c("t", "n")) {
            cat(trend, statistic, ": ")
            Q = qdftest(p, n.sample, trend, statistic)
            target = P = pdftest(Q, n.sample, trend, statistic)
            cat(c(Q, P), checkEqualsNumeric(target, current = p), "\n")
        }
    }
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.extrapolationQuantiles = 
function()
{      
    # Extrapolation: Quantiles
    .dfTable()
    check = c(NA, NA, NA, -2.66, NA, 2.16, NA, NA)
    ans = NULL
    for (p in c(0.005, 0.010, 0.990, 0.995)) {
        for (n.sample in c(10, 25)) {
            Q = qdftest(p, n.sample)
            ans = rbind(ans, c(p, n.sample, Q))
        }
    }
    ans = cbind(ans, check)
    ans
    checkEqualsNumeric(target = ans[, 3], current = ans[, 4])

    # Extrapolation: Probabilities
    A = pdftest(q = -2.7, n.sample = 100)  # 0.01 + Warning Message
    print(A)
    B = pdftest(q =  2.3, n.sample = 100)  # 0.99 + Warning Message
    print(B)
    C = pdftest(q = -1.5, n.sample =  10)  # NA
    print(C)
    D = pdftest(q = -1.6, n.sample = Inf)  # 0.1064
    print(D)
    target = c(A, B, C, D)
    current = c(0.01, 0.99, NA, 0.1064)
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.asymptoticUnitroot = 
function()
{
    # n.sample = 0 | Inf
    # trend = c("c", "nc", "ct", "ctt"), 
    # statistic = c("t", "n")

    # Asymptotic quantiles
    tol = .Machine$double.eps^0.25
    X = c(0.05, 0.10, 0.50, 0.90, 0.95)
    
    Q = qunitroot(X, trend = "c",   statistic = "t")
    P = punitroot(Q,    trend = "c",   statistic = "t")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, trend = "nc",  statistic = "t")
    P = punitroot(Q,    trend = "nc",  statistic = "t")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, trend = "ct",  statistic = "t")
    P = punitroot(Q,    trend = "ct",  statistic = "t")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, trend = "ctt", statistic = "t")
    P = punitroot(Q,    trend = "ctt", statistic = "t")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol) 
    
    Q = qunitroot(X, trend = "c",   statistic = "n")
    P = punitroot(Q,    trend = "c",   statistic = "n")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, trend = "nc",  statistic = "n")
    P = punitroot(Q,    trend = "nc",  statistic = "n")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, trend = "ct",  statistic = "n")
    P = punitroot(Q,    trend = "ct",  statistic = "n")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, trend = "ctt", statistic = "n")
    P = punitroot(Q,    trend = "ctt", statistic = "n")   
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.finiteSizeUnitroot = 
function()
{    
    # Finite size quantiles - Sample Size = 100
    tol = .Machine$double.eps^0.25
    X = c(0.05, 0.10, 0.50, 0.90, 0.95)
    
    Q = qunitroot(X, 100, trend = "c",   statistic = "t")
    P = punitroot(Q, 100, trend = "c",   statistic = "t")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, 100, trend = "nc",  statistic = "t")
    P = punitroot(Q, 100, trend = "nc",  statistic = "t")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, 100, trend = "ct",  statistic = "t")
    P = punitroot(Q, 100, trend = "ct",  statistic = "t")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, 100, trend = "ctt", statistic = "t")
    P = punitroot(Q, 100, trend = "ctt", statistic = "t")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, 100, trend = "c",   statistic = "n")
    P = punitroot(Q, 100, trend = "c",   statistic = "n")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, 100, trend = "nc",  statistic = "n")
    P = punitroot(Q, 100, trend = "nc",  statistic = "n")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)
    
    Q = qunitroot(X, 100, trend = "ct",  statistic = "n")
    P = punitroot(Q, 100, trend = "ct",  statistic = "n")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol) 
    
    Q = qunitroot(X, 100, trend = "ctt", statistic = "n")
    P = punitroot(Q, 100, trend = "ctt", statistic = "n")
    print(cbind(Q, P))
    checkEqualsNumeric(target = X, current = P, tolerance = tol)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fSeries/test/runit2A.R")
    printTextProtocol(testResult)
}
   

################################################################################
    
