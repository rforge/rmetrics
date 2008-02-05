
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
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 DESCRIPTION:
#  .runitTest                Perorms RUnit Tests
#  .rmetricsPackages         Lists all Rmetrics packages
#  .rmetricsUnitTest         Performs RUnit tests for all Rmetrics packages
################################################################################


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


################################################################################

