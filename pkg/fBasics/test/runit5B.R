
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
#  normalTest            Test suite for normality tests
#  ksnormTest            One sample Kolmogorov-Smirnov normality test
#  shapiroTest           Shapiro-Wilk normality test
#  jarqueberaTest        Jarque-Bera normality test
#  dagoTest              D'Agostino normality test
#   .skewness.test        ... internal function
#   .kurtosis.test        ... internal function
#   .omnibus.test         ... internal function
# FUNCTION:             FROM NORTEST PACKAGE:
#  adTest                Anderson-Darling normality test
#  cvmTest               Cramer-von Mises normality test
#  lillieTest            Lilliefors (Kolmogorov-Smirnov) normality test 
#  pchiTest              Pearson chi-square normality test 
#  sfTest                Shapiro-Francia normality test     
# FUNCTION:             MORE TESTS ...
#  runsTest              Runs test for detecting non-randomness [tseries]
#  gofnorm               Reports on several tests of normality
# FUNCTION ADDON:       DESCRIPTION:
#  jbTable               Table of finite sample p values for the JB test
#  pjb                   Computes probabilities for the Jarque Bera Test
#  qjb                   Computes quantiles for the Jarque Bera Test
#  .jb.test               S3 version type finite sample adjusted JB test
#  jbTest                Performs finite sample adjusted JB LM and ALM test
################################################################################
  
 
test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(OneSampleTest); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
  

test.normalTests = 
function()
{   
    set.seed(1953)
    X = rnorm(50)
    
    checkIdentical(
        target = as.character(class(try(ksnormTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(shapiroTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(jarqueberaTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(jbTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(dagoTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(adTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(cvmTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(lillieTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(pchiTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(sfTest(X)))),
        current = "fHTEST")

    runsTest(X)
    
    gofnorm(X)
}  


# ------------------------------------------------------------------------------ 
    

test.normalTestsTS = 
function()
{      
    require(fCalendar)
    data(DowJones30)
    DJ = as.timeSeries(DowJones30)
    X = returnSeries(DJ[, "IBM"])
    
    checkIdentical(
        target = as.character(class(try(ksnormTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(shapiroTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(jarqueberaTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(jbTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(dagoTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(adTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(cvmTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(lillieTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(pchiTest(X)))),
        current = "fHTEST")
        
    checkIdentical(
        target = as.character(class(try(sfTest(X)))),
        current = "fHTEST")

    runsTest(X)
    
    gofnorm(X)
}


# ------------------------------------------------------------------------------
    

if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fBasics/test/runit015D.R")
    printTextProtocol(testResult)
}
    

################################################################################    
 
    