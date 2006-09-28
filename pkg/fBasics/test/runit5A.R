
################################################################################
# FUNCTION:             NORMALITY TESTS:
#  fHTEST                Class Representation
#  show.fHTEST           S4 Print Method
# FUNCTION:             DESCRIPTION:
#  jbTable               Finite sample p values for the Jarque Bera test
# FUNCTION:             PVALUE AND STATISTICS TABLES:
#  pPlot                 General finite sample probability plot
#  pTable                Interpolated probabilities from finite sample table
#  .pTable                Utility function called by the function 'pTable'
#  qTable                Interpolated quantiles from finite sample table
#  .qTable                Utility function called by the function 'qTable'
# FUNCTION:             INTERNAL FUNCTIONS:
#  .interpTable.old      'akima' interpolation utility function
#  .interpTable.new      'akima' interpolation utility function
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(HypothesisTesting); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.jbTable.LM = 
function()
{
    # Interpolated plot of Small Jarque Bera Table:
    X = jbTable(type = "LM", size = "small")
    
    # List of finite Sizes:
    colnames(X) 
    
    # List of Probabilities:
    rownames(X)
    
    # 3D-Plot Probability Table:
    par(ask = FALSE)
    par(mfrow = c(1, 1))
    pPlot(X, linear = TRUE,  logStat = TRUE, main = "JB LM", cex = 0.5)
    pPlot(X, linear = TRUE,  logStat = TRUE, fill = TRUE,    cex = 0.5)
    pPlot(X, linear = FALSE, logStat = TRUE,                 cex = 0.5)
    pPlot(X, linear = FALSE, logStat = TRUE, fill = TRUE,    cex = 0.5)
    
    # 2D-Plot Probability for Fixed Size:
    par(mfrow = c(2, 2), cex = 0.7)
    p = (1:99)/100
    plot(qTable(X, p, N = 100), p, type = "b")
    grid()
    
    # 2D-Plot Statistics for Fixed Size:
    Stat = seq(0.01, 15, length = 100)
    plot(Stat, pTable(X, Stat, N = 100), type = "b")  
    grid()
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.jbTable.ALM = 
function()
{
    # Interpolated plot of Small Jarque Bera Table:
    X = jbTable(type = "ALM", size = "small")
    
    # List of finite Sizes:
    colnames(X) 
    
    # List of Probabilities:
    rownames(X)
    
    # 3D-Plot Probability Table:
    par(ask = FALSE)
    par(mfrow = c(1, 1))
    pPlot(X, linear = TRUE,  logStat = TRUE, main = "JB ALM", cex = 0.5)
    pPlot(X, linear = TRUE,  logStat = TRUE, fill = TRUE,     cex = 0.5)
    pPlot(X, linear = FALSE, logStat = TRUE,                  cex = 0.5)
    pPlot(X, linear = FALSE, logStat = TRUE, fill = TRUE,     cex = 0.5)
    
    # 2D-Plot Probability for Fixed Size:
    par(ask = FALSE)
    par(mfrow = c(2, 2), cex = 0.7)
    p = (1:99)/100
    plot(qTable(X, p, N = 100), p, type = "b")
    grid()
    
    # 2D-Plot Statistics for Fixed Size:
    Stat = seq(0.01, 15, length = 100)
    plot(Stat, pTable(X, Stat, N = 100), type = "b")  
    grid()
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------

 
test.adfTable = 
function()
{  
    # Dickey-Fuller Tables:
    
    # type = "ns" 
    adfTable = cbind(
        c(-2.66, -2.26, -1.95, -1.60, +0.92, +1.33, +1.70, +2.16),
        c(-2.62, -2.25, -1.95, -1.61, +0.91, +1.31, +1.66, +2.08),
        c(-2.60, -2.24, -1.95, -1.61, +0.90, +1.29, +1.64, +2.03),
        c(-2.58, -2.23, -1.95, -1.62, +0.89, +1.29, +1.63, +2.01),
        c(-2.58, -2.23, -1.95, -1.62, +0.89, +1.28, +1.62, +2.00),
        c(-2.58, -2.23, -1.95, -1.62, +0.89, +1.28, +1.62, +2.00))
    colnames(adfTable) = c(25, 50, 100, 250, 500, 100000)
    rownames(adfTable) = c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)
    print(adfTable)
        #          25    50   100   250   500 1e+05
        # 0.01  -2.66 -2.62 -2.60 -2.58 -2.58 -2.58
        # 0.025 -2.26 -2.25 -2.24 -2.23 -2.23 -2.23
        # 0.05  -1.95 -1.95 -1.95 -1.95 -1.95 -1.95
        # 0.1   -1.60 -1.61 -1.61 -1.62 -1.62 -1.62
        # 0.9    0.92  0.91  0.90  0.89  0.89  0.89
        # 0.95   1.33  1.31  1.29  1.29  1.28  1.28
        # 0.975  1.70  1.66  1.64  1.63  1.62  1.62
        # 0.99   2.16  2.08  2.03  2.01  2.00  2.00
    
    # Table:
    X = adfTable
    
    # List of finite Sizes:
    colnames(X) 
    
    # List of Probabilities:
    rownames(X)     
       
    # 3D Probability Plot:
    par(ask = FALSE)
    par(mfrow = c(1, 1))
    pPlot(X)
    ans = pPlot(X, 10, 10, fill = TRUE)$z

    # Probabilities:
    Stat = c(-3, -1.5, 0, 1.5, 3)
    N = 400
    p = pTable(X = adfTable, Stat, N) 
    names(p) = paste("q=", Stat, sep = "")
    attr(p, "N")<-N
    p
    
    # Quantiles:
    p = c(0.001, 0.01, 0.02, 0.06, 0.11)
    N = 400
    q = qTable(X = adfTable, p, N)
    names(q) = paste("p=", p, sep = "")
    attr(q, "N")<-N
    q
    
    # Return Value:
    return()
}

   
# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fBasics/test/runit5A.R")
    printTextProtocol(testResult)
}


################################################################################

 