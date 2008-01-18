
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
# You should have received A copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 TESTING:
#  .distCheck                Checks consistency of distributions
#  .runitTest                Perorms RUnit Tests
#  .rmetricsPackages         Lists all Rmetrics packages
#  .rmetricsUnitTest         Performs RUnit tests for all Rmetrics packages
#  .listIndex                Extracts R package index
#  .listDescription          Extracts R package description
#  .listFunctions            Lists all functions in a package
#  .countFunctions           Counts all functions in a package
################################################################################


.distCheck = 
    function(fun = "norm", n = 1000, robust = TRUE, subdivisions = 100, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks consistency of distributions
    
    # Arguments:
    #   fun - name of distribution
    #   ... - distributional parameters
    
    # Examples:
    #   .distCheck("norm", mean = 1, sd = 1)
    #   .distCheck("t", df = 4)
    #   .distCheck("exp", rate = 2)
    #   .distCheck("weibull", shape = 1)

    # FUNCTION:
    
    # Distribution Functions:
    cat("\nDistribution Check for:", fun, "\n ")
    CALL = match.call()
    cat("Call: ")
    cat(paste(deparse(CALL), sep = "\n", collapse = "\n"), "\n", sep = "") 
    dfun = match.fun(paste("d", fun, sep = ""))
    pfun = match.fun(paste("p", fun, sep = ""))
    qfun = match.fun(paste("q", fun, sep = ""))
    rfun = match.fun(paste("r", fun, sep = ""))
    
    # Range:
    xmin = qfun(p = 0.01, ...)
    xmax = qfun(p = 0.99, ...)
    
    # Check 1 - Normalization:
    NORM = integrate(dfun, lower = -Inf, upper = Inf, 
        subdivisions = subdivisions, stop.on.error = FALSE, ...)
    cat("\n1. Normalization Check:\n NORM ")
    print(NORM)
    normCheck = (abs(NORM[[1]]-1) < 0.01)
    
    # Check 2:
    cat("\n2. [p-pfun(qfun(p))]^2 Check:\n ")
    p = c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)
    P = pfun(qfun(p, ...), ...)
    pP = round(rbind(p, P), 3)
    print(pP)
    RMSE = sd(p-P)
    print(c(RMSE = RMSE))
    rmseCheck = (abs(RMSE) < 0.0001)
    
    # Check 3:
    cat("\n3. r(", n, ") Check:\n", sep = "")
    r = rfun(n = n, ...)
    if (!robust) {
        SAMPLE.MEAN = mean(r)
        SAMPLE.VAR = var(r)
    } else {
        robustSample = MASS::cov.mcd(r, quantile.used = floor(0.95*n))
        SAMPLE.MEAN = robustSample$center
        SAMPLE.VAR = robustSample$cov[1,1]
    }
    SAMPLE = data.frame(t(c(MEAN = SAMPLE.MEAN, "VAR" = SAMPLE.VAR)), 
        row.names = "SAMPLE")
    print(signif(SAMPLE, 3))
    fun1 = function(x, ...) { x * dfun(x, ...) }
    fun2 = function(x, M, ...) { x^2 * dfun(x, ...) }   
    MEAN = integrate(fun1, lower = -Inf, upper = Inf, 
        subdivisions = 5000, stop.on.error = FALSE,...)
    cat("   X   ")
    print(MEAN)
    VAR = integrate(fun2, lower = -Inf, upper = Inf, 
        subdivisions = 5000, stop.on.error = FALSE, ...)  
    cat("   X^2 ")
    print(VAR)
    EXACT = data.frame(t(c(MEAN = MEAN[[1]], "VAR" = VAR[[1]] - MEAN[[1]]^2)),
        row.names = "EXACT ")
    print(signif(EXACT, 3))
    meanvarCheck = (abs(SAMPLE.VAR-EXACT$VAR)/EXACT$VAR < 0.1)
    cat("\n")
    
    # Done:
    ans = list(
        normCheck = normCheck, 
        rmseCheck = rmseCheck, 
        meanvarCheck = meanvarCheck)
        
    # Return Value:
    unlist(ans)
}


# ------------------------------------------------------------------------------


.runitTest <- 
    function(package = "Rmetrics")
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Performs RUnit tests
    
    # Example:
    #   .runitTest"fCalendar")
    
    # FUNCTION:

    # if(require("RUnit", quietly = TRUE)) {

    pkg = package
    library(package = pkg, character.only = TRUE)
    # if(!(exists("path") && file.exists(path)))
        path <- system.file("unitTests", package = pkg)

    # --- Testing ---

    # Define tests
    testSuite <- defineTestSuite(name = paste(pkg, "unit testing"), dirs = path)

    cat("Now have RUnit Test Suite 'testSuite' for package '", 
        pkg, "' :\n", sep='')
    str(testSuite)
    cat('', "Consider doing",
        "\t  tests <- runTestSuite(testSuite)", "\nand later",
        "\t  printTextProtocol(tests)", '', sep = "\n")
    tests <- runTestSuite(testSuite)

    if(file.access(path, 02) != 0) {
        # cannot write to path -> use writable one
        tdir <- tempfile(paste(pkg, "unitTests", sep="_"))
        dir.create(tdir)
        pathReport <- file.path(tdir, "report")
        cat("RUnit reports are written into ", tdir, "/report.(txt|html)",
            sep = "")
    } else {
        pathReport <- file.path(path, "report")
    }

    # Print TXT Report to File:
    printTextProtocol(tests)
    printTextProtocol(tests, 
        fileName = paste(pathReport, ".txt", sep = ""))
    
    # Print HTML Report to File:
    fileName = paste(pathReport, ".html", sep = "")
    printHTMLProtocol(tests, fileName = fileName)
    
    # Repair href Links:
    protocol.html = scan(file = fileName, what = character(0))
    protocol.html = gsub('href=\"', 'href=\"file://', protocol.html)
    write(protocol.html, fileName)
    
    # stop() if there are any failures i.e. FALSE to unit test.
    # This will cause R CMD check to return error and stop
    if(getErrors(tests)$nFail > 0) {
        stop("one of the unit tests failed")
    }
  
    # Check for RUnit:
    # ... do we need this ?
    cat("R package 'RUnit' cannot be loaded -- no unit tests run\n",
        "for package", pkg,"\n")
        
    # Where you can find me ...
    cat("\nHTML Report saved to", fileName, "\n\n")
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.rmetricsPackages <- 
    function()
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Lists all Rmetrics packages
    
    # FUNCTION:
    
    # All Rmetrics Packages:
    ans = c(
        "fUtilities",     
        "fEcofin",
        "fCalendar",
        "fSeries",
        "fImport",
        "fBasics",
        "fArma",
        "fGarch",
        "fNonlinear",
        "fUnitRoots",   
        "fTrading",
        "fMultivar",
        "fRegression",
        "fExtremes",
        "fCopulae",
        "fOptions",
        "fExoticOptions",
        "fAsianOptions",
        "fAssets",
        "fPortfolio")
        
    # Return Value:
    ans        
}

        
# ------------------------------------------------------------------------------


.rmetricsUnitTest =
    function()
{       
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Performs RUnit tests for all Rmetrics packages
    
    # FUNCTION:
    
    # Do Unit Tests:
    Packages = .rmetricsPackages()
    for (package in Packages) {
        cat("\n\nPackage:", package, "\n")
        .runitTest(package)
    }
    
    # Return Value:
    invisible()       
}   


# ------------------------------------------------------------------------------


.listIndex <-
    function(package = "Rmetrics")
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Extracts R package index
    
    # Example:
    #   .listIndex("fSeries")
    
    # FUNCTION:
    
    # Extract Index:
    cmd = paste("library(help =", package, ")", sep = "" )
    ans = eval(parse(text = cmd))
    name = ans$name
    parh = ans$path
    description = ans$info[[1]]
    index = ans$info[[2]]
    cat("\n", package, "Index:\n\n")
    cat(paste(" ", index), sep = "\n")    
}   


# ------------------------------------------------------------------------------


.listDescription <-
    function(package = "Rmetrics")
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Extract package description
    
    # Example:
    #   .listIndex("fSeries")
    
    # FUNCTION:
    
    # Extract Description:
    cmd = paste("library(help =", package, ")", sep = "" )
    ans = eval(parse(text = cmd))
    name = ans$name
    parh = ans$path
    description = ans$info[[1]]
    index = ans$info[[2]]
    cat("\n", package, "Description:\n\n")
    cat(paste(" ", description), sep = "\n")
}   

 
# ------------------------------------------------------------------------------                 


.listFunctions <- 
    function(package = "Rmetrics") 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Lists all functions in a package
    
    # Arguments:
    #   package - a character string, the name of the Package

    # Author:
    #   Original code from B. Ripley
    
    # FUNCTION:
    
    # List:
    if(require(package, character.only = TRUE)) {
        env <- paste("package", package, sep = ":")
        nm <- ls(env, all = TRUE)
        ans = nm[unlist(lapply(nm, function(n) exists(n, where = env,
            mode = "function", inherits = FALSE)))]
    } else {
        ans = character(0)
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.countFunctions <- 
    function(package) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Counts all functions in a package
    
    # Arguments:
    #   package - a character string, the name of the Package
    
    # FUNCTION:
    
    # Count:
    ans = length(.listFunctions(package))
    names(ans) = package
    
    # Return Value:
    ans
}


################################################################################

